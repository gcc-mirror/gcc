#[lang = "sized"]
pub trait Sized {}

pub trait Foo<A> {
    fn foo(self, _: A) -> u16;
}

impl Foo<u16> for u16 {
    fn foo(self, _: u16) -> u16 {
        self
    }
}

impl Foo<u8> for u16 {
    fn foo(self, _: u8) -> u16 {
        self
    }
}

pub fn bar() -> u16 {
    <u16 as Foo<u16>>::foo(0u16, 0u16)
}
