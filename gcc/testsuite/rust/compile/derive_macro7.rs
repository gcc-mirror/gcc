#![feature(rustc_attrs)]

#[rustc_builtin_macro]
#[stable(feature = "rust1", since = "1.0.0")]
#[allow_internal_unstable(core_intrinsics, libstd_sys_internals)]
pub macro RustcDecodable($item:item) {
    /* compiler built-in */
}