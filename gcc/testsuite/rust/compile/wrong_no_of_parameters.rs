// https://doc.rust-lang.org/error_codes/E0061.html
fn main() {
    fn f(u: i32) {}
    fn T(u: i32, v: i32, w: i32, x: i32, y: i32, z: i32) {}

    f(); // { dg-error "this function takes 1 argument but 0 arguments were supplied" }

    T(1, 2, 3, 4, 5, 6, 7, 8, 9); // { dg-error "this function takes 6 arguments but 9 arguments were supplied" }
}
