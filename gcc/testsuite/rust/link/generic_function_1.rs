#[lang = "sized"]
pub trait Sized {}

pub fn generic_function<X>(a: X) -> X {
    a
}
