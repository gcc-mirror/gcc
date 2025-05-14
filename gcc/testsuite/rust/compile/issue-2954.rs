#[lang = "sized"]
trait Sized {}

#[lang = "receiver"]
#[unstable(feature = "receiver_trait", issue = "none")]
// #[doc(hidden)]
pub trait Receiver {
    // Empty.
}

#[unstable(feature = "receiver_trait", issue = "none")]
impl<T: ?Sized> Receiver for &T {}

#[unstable(feature = "receiver_trait", issue = "none")]
impl<T: ?Sized> Receiver for &mut T {}


