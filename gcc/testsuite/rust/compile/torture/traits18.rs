#[lang = "sized"]
pub trait Sized {}

trait Foo<'a> {}

trait Bar {
    type Item: for<'a> Foo<'a>;
}
