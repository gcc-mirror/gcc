// { dg-options "-w" }
enum Empty {}

fn foo(x: Empty) {
    match x {
        // empty
    }
}
