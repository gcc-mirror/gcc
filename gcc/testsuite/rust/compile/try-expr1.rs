// { dg-additional-options "-frust-compile-until=typecheck" }

#[lang = "sized"]
trait Sized {}

enum Result {
    #[lang = "Ok"]
    Ok(i32),
    #[lang = "Err"]
    Err(i32)
}

pub trait From<T>: Sized {
    /// Performs the conversion.
    #[lang = "from"]
    #[stable(feature = "rust1", since = "1.0.0")]
    fn from(_: T) -> Self;
}

impl<T> From<T> for T {
    fn from(t: T) -> Self { t }
}

#[lang = "try"]
pub trait Try {
    /// The type of this value when viewed as successful.
    // #[unstable(feature = "try_trait", issue = "42327")]
    // type Ok;
    /// The type of this value when viewed as failed.
    // #[unstable(feature = "try_trait", issue = "42327")]
    // type Error;

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
    fn into_result(self) -> Result;

    /// Wrap an error value to construct the composite result. For example,
    /// `Result::Err(x)` and `Result::from_error(x)` are equivalent.
    #[lang = "from_error"]
    #[unstable(feature = "try_trait", issue = "42327")]
    fn from_error(v: i32) -> Self;

    /// Wrap an OK value to construct the composite result. For example,
    /// `Result::Ok(x)` and `Result::from_ok(x)` are equivalent.
    #[lang = "from_ok"]
    #[unstable(feature = "try_trait", issue = "42327")]
    fn from_ok(v: i32) -> Self;
}

impl Try for Result {
    // type Ok = i32;
    // type Error = i32;

    fn into_result(self) -> Result {
        self
    }

    fn from_ok(v: i32) -> Self {
        Result::Ok(v)
    }

    fn from_error(v: i32) -> Self {
        Result::Err(v)
    }
}

fn bar() -> Result {
    Result::Ok(15)
}

fn foo() -> Result {
    let a = bar()?;

    Result::Ok(a)
}
