struct Foo(i32);
impl Foo {
    fn test() {}
}

pub fn main() {
    let a;
    a = Foo(123);

    a.test();
    // { dg-error "no method named .test. found in the current scope" "" { target *-*-* } .-1 }
}
