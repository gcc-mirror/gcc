# DMD Source code

This is the source code to the DMD compiler
for the D Programming Language defined in the documents at
https://dlang.org/

These sources are free, they are redistributable and modifiable
under the terms of the Boost Software License, Version 1.0.
The terms of this license are in the file boostlicense.txt,
or see https://www.boost.org/LICENSE_1_0.txt.

If a particular file has a different license in it, that overrides
this license for that file.

-Walter Bright

## Directory structure

| Folder                                                                   | Purpose                                                                                                                                                                                                       |
|--------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [dmd/](https://github.com/dlang/dmd/tree/master/compiler/src/dmd)                 | The dmd driver and front-end                                                                                                                                                                                  |
| [dmd/backend/](https://github.com/dlang/dmd/tree/master/compiler/src/dmd/backend) | Code generation for x86 or x86-64. Based on [DMC](https://github.com/DigitalMars/Compiler/)'s backend, but not kept in sync anymore. Not used by [LDC](https://github.com/ldc-developers/ldc) or [GDC](https://gdcproject.org/). |
| [dmd/common/](https://github.com/dlang/dmd/tree/master/compiler/src/dmd/common)   | Code shared by the front-end and back-end                                                                                                                                                                     |
| [dmd/root/](https://github.com/dlang/dmd/tree/master/compiler/src/dmd/root)       | Meant as a portable utility library, but ["it wasn't very good and the only project left using it is dmd"](https://github.com/dlang/dmd/pull/9844#issuecomment-498479516).                                    |

DMD has a mostly flat directory structure, so this section aims to divide all source files into logical groups for easier navigation.
The groups are roughly ordered by how late they appear in the compilation process.
Note that these groups have no strict meaning, the category assignments are a bit subjective.

### Driver

| File                                                                        | Purpose                                                               |
|-----------------------------------------------------------------------------|-----------------------------------------------------------------------|
| [main.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/main.d)           | The entry point. Contains `main`.                                     |
| [mars.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/mars.d)           | Argument parsing, path manipulation.                                  |
| [cli.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/cli.d)             | Define the command line interface                                     |
| [dmdparams.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dmdparams.d) | DMD-specific parameters                                               |
| [globals.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/globals.d)     | Define a structure storing command line options                       |
| [dinifile.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dinifile.d)   | Parse settings from .ini file (`sc.ini` / `dmd.conf`)                 |
| [vsoptions.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/vsoptions.d) | Detect the Microsoft Visual Studio toolchain for linking              |
| [frontend.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/frontend.d)   | An interface for using DMD as a library                               |
| [errors.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/errors.d)       | Error reporting implementation                                        |
| [errorsink.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/errorsink.d) | Error reporting interface                                             |
| [sarif.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/sarif.d)         | Generates SARIF reports for errors and warnings.                      |
| [target.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/target.d)       | Manage target-specific parameters for cross-compiling (for LDC/GDC)   |
| [compiler.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/compiler.d)   | Describe a back-end compiler and implements compiler-specific actions |
| [deps.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/deps.d)           | Implement the `-deps` and `-makedeps` switches                        |
| [timetrace.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/timetrace.d) | Build time profiling utility                                          |

### Lexing / parsing

| File                                                                  | Purpose                                                              |
|-----------------------------------------------------------------------|----------------------------------------------------------------------|
| [lexer.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/lexer.d)   | Convert source code into tokens for the D and ImportC parsers        |
| [location.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/location.d)| Encapsulate file/line/column info for error messages, etc.        |
| [entity.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/entity.d) | Define "\\&Entity;" escape sequence for strings / character literals |
| [tokens.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/tokens.d) | Define lexical tokens.                                               |
| [parse.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/parse.d)   | D parser, converting tokens into an Abstract Syntax Tree (AST)       |
| [cparse.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/cparse.d) | ImportC parser, converting tokens into an Abstract Syntax Tree (AST) |

### Semantic analysis

**Symbols and declarations**

| File                                                                            | Purpose                                                                                                          |
|---------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------|
| [dsymbol.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dsymbol.d)         | Base class for a D symbol, e.g. a variable, function, module, enum etc.                                          |
| [identifier.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/identifier.d)   | Represents the name of a `Dsymbol`                                                                               |
| [id.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/id.d)                   | Define strings for pre-defined identifiers (e.g. `sizeof`, `string`)                                             |
| [dscope.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dscope.d)           | Define a 'scope' on which symbol lookup can be performed                                                         |
| [dtemplate.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dtemplate.d)     | A template declaration or instance                                                                               |
| [dmodule.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dmodule.d)         | Define a package and module                                                                                      |
| [mtype.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/mtype.d)             | Define expression types such as `int`, `char[]`, `void function()`                                               |
| [arraytypes.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/arraytypes.d)   | For certain Declaration nodes of type `T`, provides aliases for `Array!T`                                        |
| [declaration.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/declaration.d) | Misc. declarations of `alias`, variables, type tuples, `ClassInfo` etc.                                          |
| [denum.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/denum.d)             | Defines `enum` declarations and enum members                                                                     |
| [attrib.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/nogc.d)             | Declarations of 'attributes' such as `private`, `pragma()`, `immutable`, `@UDA`, `align`, `extern(C++)` and more |
| [func.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/func.d)               | Define a function declaration (includes function literals, `invariant`, `unittest`)                              |
| [dversion.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dversion.d)       | Defines a version symbol, e.g. `version = ident`, `debug = ident`                                                |

**AST nodes**

| File                                                                              | Purpose                                                     |
|-----------------------------------------------------------------------------------|-------------------------------------------------------------|
| [ast_node.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/ast_node.d)         | Define an abstract AST node class                           |
| [astbase.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/astbase.d)           | Namespace of AST nodes that can be produced by the parser   |
| [astcodegen.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/astcodegen.d)     | Namespace of AST nodes of a AST ready for code generation   |
| [astenums.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/astenums.d)         | Enums common to DMD and AST                                 |
| [expression.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/expression.d)     | Define expression AST nodes                                 |
| [rootobject.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/rootobject.d)     | Define an abstract root class                           |
| [statement.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/statement.d)       | Define statement AST nodes                                  |
| [staticassert.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/staticassert.d) | Define a `static assert` AST node                           |
| [aggregate.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/aggregate.d)       | Define an aggregate (`struct`, `union` or `class`) AST node |
| [dclass.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dclass.d)             | Define a `class` AST node                                   |
| [dstruct.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dstruct.d)           | Define a `struct` or `union` AST node                       |
| [init.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/init.d)                 | Define variable initializers                                |

**AST visitors**

| File                                                                                                      | Purpose                                                                          |
|-----------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------|
| [visitor/parsetime.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/visitor/parsetime.d)                 | General [visitor](https://en.wikipedia.org/wiki/Visitor_pattern) for AST nodes   |
| [visitor/permissive.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/visitor/permissive.d)               | Subclass of ParseTimeVisitor that does not `assert(0)` on unimplemented nodes    |
| [visitor/strict.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/visitor/strict.d)                       | Visitor that forces derived classes to implement `visit` for every possible node |
| [visitor/package.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/visitor/package.d)                                   | A visitor implementing `visit` for all nodes present in the compiler             |
| [visitor/transitive.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/visitor/transitive.d)               | Provide a mixin template with visit methods for the parse time AST               |
| [visitor/postorder.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/visitor/postorder.d)                 | Depth-first expression & statement visitor                                                   |
| [visitor/statement_rewrite_walker.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/visitor/statement_rewrite_walker.d) | Statement visitor that allows replacing the currently visited node               |
| [visitor/foreachvar.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/visitor/foreachvar.d)   | Used in `ob.d` to iterate over all variables in an expression |

**Semantic passes**

| File                                                                                      | Purpose                                                           |
|-------------------------------------------------------------------------------------------|-------------------------------------------------------------------|
| [attribsem.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/attribsem.d)               | Attribute semantics                                               |
| [dsymbolsem.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dsymbolsem.d)             | Do semantic 1 pass (symbol identifiers/types)                     |
| [enumsem.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/enumsem.d)                   | Enum semantics                                                    |
| [funcsem.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/funcsem.d)                   | Function semantics                                                |
| [semantic2.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/semantic2.d)               | Do semantic 2 pass (symbol initializers)                          |
| [semantic3.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/semantic3.d)               | Do semantic 3 pass (function bodies)                              |
| [inline.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/inline.d)                     | Do inline pass (optimization pass that dmd does in the front-end) |
| [inlinecost.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/inlinecost.d)             | Compute the cost of inlining a function call.                     |
| [expressionsem.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/expressionsem.d)       | Do semantic analysis for expressions                              |
| [statementsem.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/statementsem.d)         | Do semantic analysis for statements                               |
| [initsem.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/initsem.d)                   | Do semantic analysis for initializers                             |
| [pragmasem.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/pragmasem.d)               | Do semantic analysis for pragmas                                  |
| [templatesem.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/templatesem.d)           | Do semantic analysis for templates                                |
| [templateparamsem.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/templateparamsem.d) | Do semantic analysis for template parameters                      |
| [typesem.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/typesem.d)                   | Do semantic analysis for types                                    |

**Semantic helpers**

| File                                                                          | Purpose                                                                                    |
|-------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------|
| [opover.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/opover.d)         | Operator overloading                                                                       |
| [clone.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dsymbolsem.d)      | Generate automatic `opEquals`, `opAssign` and constructors for structs                     |
| [blockexit.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/blockexit.d)   | Find out in what ways control flow can exit a block                                        |
| [ctorflow.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/ctorflow.d)     | Control flow in constructors                                                               |
| [constfold.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/constfold.d)   | Do constant folding of arithmetic expressions                                              |
| [optimize.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/optimize.d)     | Do constant folding more generally                                                         |
| [dcast.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dcast.d)           | Implicit or explicit cast(), finding common types e.g. in `x ? a : b`, integral promotions |
| [impcnvtab.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/impcnvtab.d)   | Define an implicit conversion table for basic types                                        |
| [importc.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/importc.d)       | Helpers specific to ImportC                                                                |
| [sideeffect.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/sideeffect.d) | Extract side-effects of expressions for certain lowerings.                                 |
| [mustuse.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/mustuse.d)       | Helpers related to the `@mustuse` attribute                                                |


**Compile Time Function Execution (CTFE)**

| File                                                                          | Purpose                                                                             |
|-------------------------------------------------------------------------------|-------------------------------------------------------------------------------------|
| [dinterpret.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dinterpret.d) | CTFE entry point                                                                    |
| [ctfeexpr.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/ctfeexpr.d)     | CTFE for expressions involving pointers, slices, array concatenation etc.           |
| [builtin.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/builtin.d)       | Allow CTFE of certain external functions (`core.math`, `std.math` and `core.bitop`) |

### Specific language features

**Attribute checks**

| File                                                                      | Purpose                                |
|---------------------------------------------------------------------------|----------------------------------------|
| [nogc.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/nogc.d)         | `@nogc` checks                         |
| [safe.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/safe.d)         | `@safe` checks                         |
| [canthrow.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/canthrow.d) | `nothrow` checks                       |
| [escape.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/escape.d)     | `scope` checks                         |
| [access.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/access.d)     | `public` / `private` checks            |
| [ob.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/ob.d)             | Ownership / borrowing (`@live`) checks |

**Inline Assembly**

| File                                                                    | Purpose                                   |
|-------------------------------------------------------------------------|-------------------------------------------|
| [iasm.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/iasm.d)       | Inline assembly depending on the compiler |
| [iasmdmd.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/iasmdmd.d) | Inline assembly for DMD                   |
| [iasmgcc.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/iasmgcc.d) | Inline assembly for GDC                   |

**Other**

| File                                                                           | Purpose                                                                                     |
|--------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------|
| [aliasthis.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/aliasthis.d)    | Resolve implicit conversions for `alias X this`                                             |
| [traits.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/traits.d)          | `__traits()`                                                                                |
| [lambdacomp.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/lambdacomp.d)  | `__traits(isSame, x => y, z => w)`                                                          |
| [cond.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/cond.d)              | Evaluate `static if`, `version` `debug `                                                    |
| [staticcond.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/staticcond.d)  | Lazily evaluate static conditions for `static if`, `static assert` and template constraints |
| [delegatize.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/delegatize.d)  | Converts expression to delegates for `lazy` parameters                                      |
| [nspace.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/nspace.d)          | Namespace for `extern (C++, Module)`                                                        |
| [intrange.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/intrange.d)      | [Value range propagation](https://digitalmars.com/articles/b62.html)                        |
| [dimport.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dimport.d)        | Renamed imports (`import aliasSymbol = pkg1.pkg2.symbol`)                                   |
| [arrayop.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/arrayop.d)        | Array operations (`a[] = b[] + c[]`)                                                        |
| [cpreprocess.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/cpreprocess.d)| Run the C preprocessor on C source files                                                   |
| [typinf.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/typinf.d)          | Generate typeinfo for `typeid()` (as well as internals)                                     |

| File                                                                        | Purpose                                                                            |
|-----------------------------------------------------------------------------|------------------------------------------------------------------------------------|
| [chkformat.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/chkformat.d) | Validate arguments with format specifiers for `printf` / `scanf` etc.              |
| [imphint.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/imphint.d)     | Give a suggestion to e.g. `import std.stdio` when `writeln` could not be resolved. |

### Library files

| File                                                                          | Purpose                                              |
|-------------------------------------------------------------------------------|------------------------------------------------------|
| [lib/package.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/lib/package.d)               | Abstract library class                               |
| [lib/elf.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/lib/elf.d)         | Library in ELF format (Unix)                         |
| [lib/mach.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/lib/mach.d)       | Library in Mach-O format (macOS)                     |
| [lib/mscoff.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/lib/mscoff.d)   | Library in COFF format (32/64-bit Windows)           |
| [lib/scanelf.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/lib/scanelf.d)       | Extract symbol names from a library in ELF format    |
| [lib/scanmach.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/lib/scanmach.d)     | Extract symbol names from a library in Mach-O format |
| [lib/scanmscoff.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/lib/scanmscoff.d) | Extract symbol names from a library in COFF format   |

### Code generation / back-end interfacing

| File                                                                                        | Purpose                                                                             |
|---------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------|
| [dmsc.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dmsc.d)                           | Configures and initializes the back-end                                             |
| [toobj.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/toobj.d)                         | Convert an AST that went through all semantic phases into an object file            |
| [toir.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/toir.d)                           | Convert Dsymbols intermediate representation                                        |
| [e2ir.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/e2ir.d)                           | Convert Expressions to intermediate representation                                  |
| [s2ir.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/s2ir.d)                           | Convert Statements to intermediate representation                                   |
| [stmtstate.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/stmtstate.d)                 | Used to help transform statement AST into flow graph                                |
| [toctype.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/toctype.d)                     | Convert a D type to a type the back-end understands                                 |
| [tocsym.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/tocsym.d)                       | Convert a D symbol to a symbol the linker understands (with mangled name)           |
| [argtypes_x86.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/argtypes_x86.d)           | Convert a D type into simple (register) types for the 32-bit x86 ABI                |
| [argtypes_sysv_x64.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/argtypes_sysv_x64.d) | 'argtypes' for the x86_64 System V ABI                                              |
| [argtypes_aarch64.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/argtypes_aarch64.d)   | 'argtypes' for the AArch64 ABI                                                      |
| [glue.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/glue.d)                           | Generate the object file for function declarations                                  |
| [gluelayer.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/gluelayer.d)                 | Declarations for back-end functions that the front-end invokes                      |
| [todt.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/todt.d)                           | Convert initializers into structures that the back-end will add to the data segment |
| [tocvdebug.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/tovcdebug.d)                 | Generate debug info in the CV4 debug format.                                        |
| [objc.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/objc.d)                           | Objective-C interfacing                                                             |
| [objc_glue.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/objc_glue.d)                 | Glue code for Objective-C interop.                                                  |

**Name mangling**

| File                                                                              | Purpose                                                          |
|-----------------------------------------------------------------------------------|------------------------------------------------------------------|
| [mangle/cpp.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/mangle/cpp.d)       | C++ name mangling                                                |
| [mangle/cppwin.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/mangle/cppwin.d) | C++ name mangling for Windows                                    |
| [mangle/basic.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/mangle/basic.d)   | D name mangling for basic types                                  |
| [mangle/package.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/mangle/package.d)           | D [name mangling](https://dlang.org/spec/abi.html#name_mangling) |

### Linking

| File                                                              | Purpose                                 |
|-------------------------------------------------------------------|-----------------------------------------|
| [link.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/link.d) | Invoke the linker as a separate process |

### Special output

| File                                                                  | Purpose                                                                                                 |
|-----------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------|
| [doc.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/doc.d)       | [Documentation generation](https://dlang.org/spec/ddoc.html)                                            |
| [dmacro.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dmacro.d) | DDoc macro processing                                                                                   |
| [hdrgen.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/hdrgen.d) | Convert an AST into D source code for `.di` header generation, as well as `-vcg-ast` and error messages |
| [json.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/json.d)     | Describe the module in a `.json` file for the `-X` flag                                                 |
| [dtoh.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dtoh.d)     | C++ header generation from D source files                                                               |
| [disasm86.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/backend/x86/disasm86.d)       | x86-64 disassembly generation
| [disasmarm.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/backend/arm/disasmarm.d) | AArch64 disassembly generation

### Utility

Note: many other utilities are in [dmd/root](https://github.com/dlang/dmd/tree/master/compiler/src/dmd/root).

| File                                                                              | Purpose                                           |
|-----------------------------------------------------------------------------------|---------------------------------------------------|
| [console.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/console.d)           | Print error messages in color                     |
| [file_manager.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/file_manager.d) | Keep file contents in memory                      |
| [utils.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/utils.d)               | Utility functions related to files and file paths |

| File                                                                            | Purpose                                                       |
|---------------------------------------------------------------------------------|---------------------------------------------------------------|
| [asttypename.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/asttypename.d) | Print the internal name of an AST node (for debugging only)   |
| [printast.d](https://github.com/dlang/dmd/blob/master/compiler/src/dmd/printast.d)       | Print the AST data structure                                  |
