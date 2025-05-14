#[lang = "sized"]
trait Sized {}

enum Result<T, E> {
    #[lang = "Ok"]
    Ok(T),
    #[lang = "Err"]
    Err(E),
}

#[lang = "try"]
pub trait Try {
    /// The type of this value when viewed as successful.
    // #[unstable(feature = "try_trait", issue = "42327")]
    type Ok;
    /// The type of this value when viewed as failed.
    // #[unstable(feature = "try_trait", issue = "42327")]
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
    fn from_error(v: Self::Ok) -> Self;

    /// Wrap an OK value to construct the composite result. For example,
    /// `Result::Ok(x)` and `Result::from_ok(x)` are equivalent.
    #[lang = "from_ok"]
    #[unstable(feature = "try_trait", issue = "42327")]
    fn from_ok(v: Self::Error) -> Self;
}

impl<T, E> Try for Result<T, E> {
    type Ok = T;
    type Error = E;

    fn into_result(self) -> Result<T, E> {
        self
    }

    fn from_ok(v: T) -> Self {
        Result::Ok(v)
    }

    fn from_error(v: E) -> Self {
        Result::Err(v)
    }
}
