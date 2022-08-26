fn main() {
    let a: (i32, bool) = (123, 123); // { dg-error "expected" }
    let b;
    b = (456, 5f32);
}
