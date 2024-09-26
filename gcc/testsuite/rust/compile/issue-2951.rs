#[lang = "sized"]
pub trait Sized {}

#[lang = "clone"]
pub trait Clone: Sized {
    fn clone(&self) -> Self;
}

impl Clone for ! {
    fn clone(&self) -> Self {
        *self
    }
}
