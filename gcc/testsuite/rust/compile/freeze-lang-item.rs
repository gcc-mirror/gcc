#![feature(no_core)]
#![feature(optin_builtin_traits)]
#![feature(negative_impls)]
#![feature(lang_items)]
#![feature(rustc_attrs)]
#![no_core]

#[lang = "sized"]
trait Sized {}

#[lang = "freeze"]
pub(crate) unsafe auto trait Freeze {}

#[lang = "phantom_data"]
pub struct PhantomData<T>;

pub struct UnsafeCell<T>(T);

impl<T: ?Sized> !Freeze for UnsafeCell<T> {}
unsafe impl<T: ?Sized> Freeze for PhantomData<T> {}
unsafe impl<T: ?Sized> Freeze for *const T {}
unsafe impl<T: ?Sized> Freeze for *mut T {}
unsafe impl<T: ?Sized> Freeze for &T {}
unsafe impl<T: ?Sized> Freeze for &mut T {}

