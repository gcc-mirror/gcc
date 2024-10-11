#![feature(negative_impls)]

#[lang = "sized"]
trait Sized {}

#[lang = "deref"]
pub trait Deref {
    /// The resulting type after dereferencing.
    #[stable(feature = "rust1", since = "1.0.0")]
    // #[rustc_diagnostic_item = "deref_target"]
    type Target: ?Sized;

    /// Dereferences the value.
    #[must_use]
    #[stable(feature = "rust1", since = "1.0.0")]
    // #[rustc_diagnostic_item = "deref_method"]
    fn deref(&self) -> &Self::Target;
}

impl<T: ?Sized> Deref for &T {
    type Target = T;

    fn deref(&self) -> &T {
        *self
    }
}

// this is added because of #3030
extern "C" {
    fn never() -> !;
}

impl<T: ?Sized> !DerefMut for &T {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { never() }
    }
}

impl<T: ?Sized> Deref for &mut T {
    type Target = T;

    fn deref(&self) -> &T {
        *self
    }
}

#[lang = "deref_mut"]
pub trait DerefMut: Deref {
    /// Mutably dereferences the value.
    #[stable(feature = "rust1", since = "1.0.0")]
    fn deref_mut(&mut self) -> &mut Self::Target;
}

impl<T: ?Sized> DerefMut for &mut T {
    fn deref_mut(&mut self) -> &mut T {
        *self
    }
}
