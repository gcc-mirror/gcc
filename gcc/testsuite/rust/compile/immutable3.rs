fn main() {
    let a = 1;
    a += 2; // { dg-error "cannot assign to immutable" }
}
