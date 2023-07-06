// https://doc.rust-lang.org/error_codes/E0425.html
fn main() {
    let f = x * x * 3; // { dg-error "cannot find value .x. in this scope" }
    let a = f(); // invalid, too few parameters
    let b = f(4); // this works!
    let c = f(2, 3); // invalid, too many parameters
}
