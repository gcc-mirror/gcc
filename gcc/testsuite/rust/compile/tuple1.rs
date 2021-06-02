fn main() {
    let a: (i32, bool) = (123, 123); // { dg-error "expected .bool. got .<integer>." }
    let b;
    b = (456, 5f32);
}
