struct Foo(i32);

impl Foo {
    fn test() {}
}

pub fn main() {
    // { dg-error {expected \[\(\)\] got \[<tyty::error>\]} "" { target *-*-* } .-1 }
    let a;
    a = Foo(123);

    a.test()
    // { dg-error "associated function is not a method" "" { target *-*-* } .-1 }
    // { dg-error {failed to type resolve expression} "" { target *-*-* } .-2 }
}
