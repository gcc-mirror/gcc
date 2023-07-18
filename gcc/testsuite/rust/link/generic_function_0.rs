// { dg-xfail-if "https://github.com/Rust-GCC/gccrs/issues/2349" { *-*-* } }
// { dg-excess-errors "" { xfail *-*-* } }

extern crate generic_function_1;
use generic_function_1::generic_function;

fn main() -> i32 {
    let a = generic_function(123);
    a - 123
}
