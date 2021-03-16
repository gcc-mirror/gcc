static x: i32 = 3;

fn main() {
    x = 1; /* { dg-error "cannot assign to immutable" } */
}
