#[lang = "sized"]
trait Sized {}

// ---- gccrs additions

#[lang = "clone"]
pub trait Clone: Sized {
    #[stable(feature = "rust1", since = "1.0.0")]
    #[must_use = "cloning is often expensive and is not expected to have side effects"]
    fn clone(&self) -> Self;

    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    fn clone_from(&mut self, source: &Self) {
        *self = source.clone()
    }
}

#[unstable(feature = "never_type", issue = "35121")]
impl Clone for ! {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}
