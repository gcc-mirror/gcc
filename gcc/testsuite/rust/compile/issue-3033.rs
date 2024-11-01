#![feature(negative_impls)]

#[lang = "copy"]
trait Copy {}

mod copy_impls {
    use super::Copy;

    macro_rules! impl_copy {
        ($($t:ty)*) => {
            $(
                impl Copy for $t {}
            )*
        }
    }

    impl_copy! {
        usize u8 u16 u32 u64 // u128
        isize i8 i16 i32 i64 // i128
        f32 f64
        bool char
    }
}

#[stable(feature = "rust1", since = "1.0.0")]
#[repr(transparent)]
#[repr(no_niche)] // rust-lang/rust#68303.
pub struct UnsafeCell<T: ?Sized> {
    value: T,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

impl<T: ?Sized> UnsafeCell<T> {
    /// Gets a mutable pointer to the wrapped value.
    ///
    /// This can be cast to a pointer of any kind.
    /// Ensure that the access is unique (no active references, mutable or not)
    /// when casting to `&mut T`, and ensure that there are no mutations
    /// or mutable aliases going on when casting to `&T`
    ///
    /// # Examples
    ///
    ///
    /// use std::cell::UnsafeCell;
    ///
    /// let uc = UnsafeCell::new(5);
    ///
    /// let five = uc.get();
    ///
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    // #[rustc_const_stable(feature = "const_unsafecell_get", since = "1.32.0")]
    pub const fn get(&self) -> *mut T {
        // We can just cast the pointer from `UnsafeCell<T>` to `T` because of
        // #[repr(transparent)]. This exploits libstd's special status, there is
        // no guarantee for user code that this will work in future versions of the compiler!
        self as *const UnsafeCell<T> as *const T as *mut T
    }
}

#[stable(feature = "rust1", since = "1.0.0")]
#[repr(transparent)]
pub struct Cell<T: ?Sized> {
    value: UnsafeCell<T>,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

impl<T: Copy> Cell<T> {
    /// Returns a copy of the contained value.
    ///
    /// # Examples
    ///
    ///
    /// use std::cell::Cell;
    ///
    /// let c = Cell::new(5);
    ///
    /// let five = c.get();
    ///
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn get(&self) -> T {
        // SAFETY: This can cause data races if called from a separate thread,
        // but `Cell` is `!Sync` so this won't happen.
        unsafe { *self.value.get() }
    }
}

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

#[inline]
pub fn new<'b>(borrow: &'b Cell<i32>) {
    let b = borrow.get();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
