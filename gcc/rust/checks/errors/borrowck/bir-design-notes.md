# Borrow-checker IR (BIR) design notes

## Design goals

Rust GCC project aims to use the [Polonius project](https://github.com/rust-lang/polonius) as its borrow-checker.
Polonius operates on a set of [facts](https://github.com/rust-lang/polonius/blob/master/polonius-engine/src/facts.rs) about the program to determine
the actual lifetimes of borrows.
As Polonius's primary analysis is location sensitive, the facts are tied to the program's control flow graph (CFG).
Unlike rustc, which has
its
own three address language specific representation called [MIR](https://rustc-dev-guide.rust-lang.org/mir/index.html), gccrs uses gcc's AST based
representation GENERIC.
GIMPLE (the three address IR) is generated from GENERIC inside GCC and can carry no language-specific information.
Therefore, we
need to generate our own representation of the program's CFG.
Since gccrs has no intention of having a MIR-like IR, the BIR is not to be used for
code generation.
Therefore, BIR carries only minimal information that is necessary for the borrow-checker and for BIR debugging.

Since BIR is in fact a dead branch of the compilation pipeline, the only way to verify its generations is through manual inspection.
To have some frame of reference for testing, BIR build and dump are carefully designed to resemble the textual dump of rustc's MIR as much as
possible.
This includes the style of the output, numbering of locals and order of basic blocks (when possible).

## BIR Dump Example

An example program calculating the i-th fibonacci number:

```rust

fn fib(i: usize) -> i32 {
    if i == 0 || i == 1 {
        1
    } else {
        fib(i - 1) + fib(i - 2)
    }
}
```

Here is an example of BIR dump (note: this needs to be updated regularly):

```
fn fib(_1: usize) -> i32 {
    let _0: i32;
    let _2: i32;
    let _3: bool;
    let _4: bool;
    let _5: bool;
    let _6: usize;
    let _7: i32;
    let _8: usize;
    let _9: i32;
    let _10: i32;

    bb0: {
        _4 = Operator(_1, const usize);
        switchInt(_4) -> [bb1, bb2];
    }

    bb1: {
        _3 = const bool;
        goto -> bb3;
    }

    bb2: {
        _5 = Operator(_1, const usize);
        _3 = _5;
        goto -> bb3;
    }

    bb3: {
        switchInt(_3) -> [bb4, bb7];
    }

    bb4: {
        _2 = const i32;
        goto -> bb8;
    }

    bb5: {
        _6 = Operator(_1, const usize);
        _7 = Call(fib)(_6, ) -> [bb6];
    }

    bb6: {
        _8 = Operator(_1, const usize);
        _9 = Call(fib)(_8, ) -> [bb7];
    }

    bb7: {
        _10 = Operator(_7, _9);
        _2 = _10;
        goto -> bb8;
    }

    bb8: {
        _0 = _2;
        return;
    }
}


```

The dump consists of:

- A function header with arguments: `fn fib(_1: usize) -> i32 { ... }`.
- Declaration of locals: `let _0: i32;`, where `_0` is the return value (even if it is of the unit type). Arguments are not listed here, they are
  listed in the function header.
- A list of basic blocks: `bb0: { ... }`. The basic block name is the `bb` prefix followed by a number.
- Each basic block consists of a list of BIR nodes (instructions). Instruction can be either assigned to a local (place) or be a statement.
  Instructions take locals (places) as arguments.
- Each basic block is terminated with a control flow instruction followed by a list of destinations:
    - `goto -> bb3;` - a goto instruction with a single destination.
    - `switchInt(_3) -> [bb4, bb7];` - a switch instruction with multiple destinations.
    - `return;` - a return instruction with no destinations.
    - `Call(fib)(_6, ) -> [bb6];` - a call instruction with a single destination. This section is prepared for panic handling.

## BIR Structure

BIR structure is defined in `gcc/rust/checks/errors/borrowck/rust-bir.h`. It is heavily inspired by rustc's MIR. The main difference is that BIR
drastically reduces the amount of information carried to only borrow-checking relevant information.

As borrow-checking is performed on each function independently, BIR represents a single function (`struct Function`). A `Function` consists of a list
of basic blocks, list of arguments (for dump only) and place database, which keeps track of locals.

### Basic Blocks

A basic block is identified by its index in the function's basic block list. It contains a list of BIR nodes (instructions) and a list of successor
basic block indices in CFG.

### BIR Nodes (Instructions)

BIR nodes are of three categories:

- An assignment of an expression to a local (place).
- A control flow operation (switch, return).
- A special node (not executable) node, which carries additional information for borrow-checking (`StorageDead`, `StorageLive`).

#### Expressions

Expressions represent the executable parts of the rust code. Many different Rust contracts are represented by a single expression, as only data (and
lifetime) flow needs to be tracked.

- `InitializerExpr` represents any kind of struct initialization. It can be either explicit (struct expression) or implicit (range expression,
  e.g. `0..=5`).
- `Operator<ARITY>` represents any kind of operation, except the following, where special information is needed either for borrow-checking or for
  better debugging.
- `BorrowExpr` represents a borrow operation.
- `AssignmentExpr` holds a place for a node of assignment (i.e., no operation is done on the place, it is just assigned).
- `CallExpr` represents a function call.
    - For functions, the callable is represented by a constant place (see below). (E.i. all calls use the same constant place.)
    - For closures and function pointers, the callable is represented by a (non-constant) place.

### Places

Places are defined in `gcc/rust/checks/errors/borrowck/rust-bir-place.h`.

Places represent locals (variables), their field, and constants. They are identified by their index (`PlaceId`) in the function's place database. For
better dump correspondence to MIR, constants use a different index range.

Non-constant places are created according to Polonius path [documentation](https://rust-lang.github.io/polonius/rules/atoms.html). The following
grammar describes
possible path elements:

```
Path = Variable
     | Path "." Field // field access
     | Path "[" "]"   // index
     | "*" Path
```

It is important to highlight that different fields are assigned to different places; however, all indices are assigned to the same place.
Also, to match the output of rustc.
In dump, paths contain at most one dereference and are split otherwise.
Same paths always result in the same place.

Variables are identified by `AST` `NodeId`. Fields indexes are taken from `TyTy` types.

Each place holds indices to its next relatives (in the path tree), `TyTy` type, lifetime and information whether the type can be copies or it needs to
be moved. Not that unlike rustc, we copy any time we can (for simplicity), while rustc prefers to move if possible (only a single copy is held).

## BIR Builders

There are multiple builders (visitor classes) for BIR based on what context is needed in them.
provides the entry point that handles function parameters and return values, and it creates the BIR main unit `Function`.
`rust-bir-internal.h` provides abstract builder classes with common helper methods for all builder and for expression builders.
Specific builders are then defined for expressions+statements, lazy boolean expressions, patterns, and struct initialization.