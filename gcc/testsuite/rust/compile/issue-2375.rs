#[lang = "sized"]
pub trait Sized {}

pub trait Trait {
    fn foo(&self) -> Self
    where
        Self: Sized;
}

pub fn static_foo<T: Trait + ?Sized>(_b: &T) {}

pub fn dynamic_bar(a: &dyn Trait) {
    static_foo(a)
}
