extern crate simple_function_1;
use simple_function_1::test_func;

fn main() -> i32 {
    let a = test_func(123);
    // { dg-bogus "call to extern function" "" { xfail *-*-* } .-1 }
    a - 124
}
