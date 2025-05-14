// { dg-options "-w" }
enum Empty {}

fn foo(x: Empty) {
    let x: Empty = match x {
        // empty
    };
}
