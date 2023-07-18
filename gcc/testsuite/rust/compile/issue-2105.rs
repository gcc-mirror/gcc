#[lang = "sized"]
pub trait Sized {}

pub enum Option<T> {
    Some(T),
    None,
}

pub use Option::{None, Some};

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

impl<T> Option<T> {
    pub fn map<R, F: FnOnce(T) -> R>(self, f: F) -> Option<R> {
        match self {
            Some(value) => Some(f(value)),
            None => None,
        }
    }
}
