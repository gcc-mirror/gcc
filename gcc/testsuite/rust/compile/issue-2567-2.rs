// { dg-options "-w" }
enum Empty {}

fn foo(x: Empty) {
    let x: i32 = match x {
        // empty
    };
}
