// { dg-options "-w" }
union B {
    a: A,
    b: f32,
}

struct A {
    data: i32,
    len: usize,
}
