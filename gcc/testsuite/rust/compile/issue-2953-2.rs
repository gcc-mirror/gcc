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

pub fn max_by<T, F: FnOnce(&T, &T) -> Ordering>(v1: T, v2: T, compare: F) -> T {
    match compare(&v1, &v2) {
        Ordering::Less | Ordering::Equal => v2,
        Ordering::Greater => v1,
    }
}

pub fn min_by<T, F: FnOnce(&T, &T) -> Ordering>(v1: T, v2: T, compare: F) -> T {
    match compare(&v1, &v2) {
        Ordering::Less | Ordering::Equal => v1,
        Ordering::Greater => v2,
    }
}
