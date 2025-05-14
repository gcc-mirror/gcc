#![feature(negative_impls)]

#[lang = "sized"]
pub trait Sized {}

pub trait Deref {}

pub trait DerefMut: Deref {
    type Target;

    /// Mutably dereferences the value.
    #[stable(feature = "rust1", since = "1.0.0")]
    fn deref_mut(&mut self) -> &mut Self::Target;
}

impl<T: ?Sized> !DerefMut for &T {}
