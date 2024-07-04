macro_rules! numbers {
    {} => { 1 2 }
    // { dg-error "expecting .;. but .integer literal. found" "" { target *-*-* } .-1 }
}

pub fn foo() {
    numbers!();
}

fn main() -> i32 { 0 }
