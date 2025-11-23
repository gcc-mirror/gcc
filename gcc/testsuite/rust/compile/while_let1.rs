#![feature(lang_items)]

// use self::Ordering::*;
// use Ordering::*;

// enum Ordering {
//     A,
//     B,
// }

// fn foo(_: Ordering) {}

// fn main() {
//     let a = A;

//     foo(a);
//     foo(B);
// }

#[lang = "sized"]
trait Sized {}

enum Result<T, E> {
    Ok(T),
    Err(E),
}

pub trait Try {
    /// The type of this value when viewed as successful.
    #[unstable(feature = "try_trait", issue = "42327")]
    type Ok;
    /// The type of this value when viewed as failed.
    #[unstable(feature = "try_trait", issue = "42327")]
    type Error;

    /// Applies the "?" operator. A return of `Ok(t)` means that the
    /// execution should continue normally, and the result of `?` is the
    /// value `t`. A return of `Err(e)` means that execution should branch
    /// to the innermost enclosing `catch`, or return from the function.
    ///
    /// If an `Err(e)` result is returned, the value `e` will be "wrapped"
    /// in the return type of the enclosing scope (which must itself implement
    /// `Try`). Specifically, the value `X::from_error(From::from(e))`
    /// is returned, where `X` is the return type of the enclosing function.
    #[lang = "into_result"]
    #[unstable(feature = "try_trait", issue = "42327")]
    fn into_result(self) -> Result<Self::Ok, Self::Error>;

    /// Wrap an error value to construct the composite result. For example,
    /// `Result::Err(x)` and `Result::from_error(x)` are equivalent.
    #[lang = "from_error"]
    #[unstable(feature = "try_trait", issue = "42327")]
    fn from_error(v: Self::Error) -> Self;

    /// Wrap an OK value to construct the composite result. For example,
    /// `Result::Ok(x)` and `Result::from_ok(x)` are equivalent.
    #[lang = "from_ok"]
    #[unstable(feature = "try_trait", issue = "42327")]
    fn from_ok(v: Self::Ok) -> Self;
}

pub struct NoneError;

pub enum Option<T> {
    /// No value
    None,
    /// Some value `T`
    Some(T),
}

impl<T> Option<T> {
    pub fn ok_or<E>(self, err: E) -> Result<T, E> {
        match self {
            Some(ok) => Result::Ok(ok),
            None => Result::Err(err),
        }
    }
}

use Option::*;

#[unstable(feature = "try_trait", issue = "42327")]
impl<T> Try for Option<T> {
    type Ok = T;
    type Error = NoneError;

    #[inline]
    fn into_result(self) -> Result<T, NoneError> {
        self.ok_or(NoneError)
    }

    #[inline]
    fn from_ok(v: T) -> Self {
        Some(v)
    }

    #[inline]
    fn from_error(_: NoneError) -> Self {
        None
    }
}

fn foo() -> Option<i32> {
    Option::Some(15)
}

fn main() {
    // let _: Option<i32> = try { 15i32 };

    while let Option::Some(15) = foo() {}
}
