#![feature(no_core)]
#![feature(optin_builtin_traits)]
#![feature(negative_impls)]
#![feature(lang_items)]
#![feature(rustc_attrs)]
#![no_core]

#[lang = "sized"]
trait Sized {}

#[lang = "phantom_data"]
pub struct PhantomData<T>;

type c_void = ();

/// x86_64 ABI implementation of a `va_list`.
// #[cfg(all(target_arch = "x86_64", not(windows)))]
#[repr(C)]
// #[derive(Debug)]
#[unstable(
    feature = "c_variadic",
    reason = "the `c_variadic` feature has not been properly tested on \
              all supported platforms",
    issue = "44930"
)]
#[lang = "va_list"]
pub struct VaListImpl<'f> {
    pub gp_offset: i32,
    pub fp_offset: i32,
    pub overflow_arg_area: *mut c_void,
    pub reg_save_area: *mut c_void,
    pub _marker: PhantomData<&'f mut &'f c_void>,
}
