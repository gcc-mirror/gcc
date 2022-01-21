struct Foo(i32);
impl Foo {
    fn test() {}
}

pub fn main() {
    let a;
    a = Foo(123);

    a.test();
    // { dg-error "failed to resolve method for .test." "" { target *-*-* } .-1 }
    // { dg-error {failed to type resolve expression} "" { target *-*-* } .-2 }
}
