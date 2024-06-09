// https://doc.rust-lang.org/error_codes/E0571.html
#![allow(unused)]
fn main() {
    let mut i = 1;
    fn satisfied(n: usize) -> bool {
        n % 23 == 0
    }
    let result = while true {
        if satisfied(i) {
            break 2 * i;   // { dg-error "can only .break. with a value inside a .loop. block" }
        }
        i += 1;
    };
}
