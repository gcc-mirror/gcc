#[lang = "sized"]
trait Sized {}

#[stable(feature = "rust1", since = "1.0.0")]
pub trait Default: Sized {
    #[stable(feature = "rust1", since = "1.0.0")]
    fn default() -> Self;
}

impl Default for () {
    fn default() -> () {
        ()
    }
}
