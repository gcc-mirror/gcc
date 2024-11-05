#[lang = "sized"]
pub trait Sized {
    // Empty.
}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    /// The returned type after the call operator is used.
    #[lang = "fn_once_output"]
    type Output;

    /// Performs the call operation.
    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

pub enum Ordering {
    /// An ordering where a compared value is less than another.
    Less = -1,
    /// An ordering where a compared value is equal to another.
    Equal = 0,
    /// An ordering where a compared value is greater than another.
    Greater = 1,
}

pub fn f<F: FnOnce(i32) -> Ordering>(g: F) -> Ordering {
    g(1)
}
