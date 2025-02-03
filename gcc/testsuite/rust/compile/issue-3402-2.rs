pub struct Bar(i32);

#[lang = "sized"]
trait Sized {}

pub trait A: Sized {
    fn foo() -> Self;
}

impl A for i32 {
    fn foo() -> Self {
        0
    }
}

pub fn bar() {
    let _ = Bar(A::foo());
}
