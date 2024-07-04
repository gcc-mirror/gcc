#[lang = "sized"]
pub trait Sized {}

struct Foo(u32);
// { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }

pub trait Bar {
    fn bar(self);
}

impl Foo {
    pub fn map<F>(f: F)
    where
        F: Bar,
    {
        f.bar();
    }
}
