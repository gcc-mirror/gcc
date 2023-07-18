#[lang = "sized"]
pub trait Sized {}

pub enum Option<T> {
    None,
    Some(T),
}

pub enum Result<T, E> {
    Ok(T),
    Err(E),
}

pub trait TryFrom<T> {
    type Error;

    fn try_from(value: T) -> Result<Self, Self::Error>;
}

pub trait From<T> {
    fn from(_: T) -> Self;
}

impl<T> From<T> for T {
    fn from(t: T) -> T {
        t
    }
}

impl<T, U> TryFrom<U> for T
where
    T: From<U>,
{
    type Error = !;

    fn try_from(value: U) -> Result<Self, Self::Error> {
        Result::Ok(T::from(value))
    }
}

pub fn test(n: usize) {
    let _a = <usize>::try_from(n);
}
