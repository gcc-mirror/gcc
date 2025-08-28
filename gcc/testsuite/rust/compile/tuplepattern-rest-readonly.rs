fn main() {
    let (a, .., b) = (1, 1);
    a = 2; // { dg-error "assignment of read-only variable .a." }
    b = 2; // { dg-error "assignment of read-only variable .b." }
}