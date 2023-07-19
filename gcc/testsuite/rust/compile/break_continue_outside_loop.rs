// https://doc.rust-lang.org/error_codes/E0268.html
#![allow(unused)]
fn boo() {
    continue; // { dg-error ".continue. outside of a loop" }
    break; // { dg-error ".break. outside of a loop or labeled block" }
}

fn main() {
    boo()
}
