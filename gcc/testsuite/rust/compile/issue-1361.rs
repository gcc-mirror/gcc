// { dg-options "-w" }
fn foo() -> S {
    S { a: 15 }
}

struct S {
    a: i32,
}
