struct Foo(i32);

trait Bar {
    const A: i32 = 123;
    fn B();
    fn C(&self);
}

pub fn main() {
    let a;
    a = Foo(123);

    let b: &dyn Bar = &a;
    // { dg-error "trait bound is not object safe" "" { target *-*-* } .-1 }
}
