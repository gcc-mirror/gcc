#[lang = "sized"]
pub trait Sized {}

pub enum Result<T, E> {
    Ok(T),
    Err(E),
}

pub mod module {
    pub struct E;
}

pub fn foo() -> Result<(), module::E> {
    Result::Err(module::E)
}
