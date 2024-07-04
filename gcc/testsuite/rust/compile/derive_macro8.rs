#![feature(rustc_attrs)]
#![feature(decl_macro)]

#[rustc_builtin_macro]
pub macro Copy($i:item) { /* builtin */ }

pub fn foo() {
    Copy!(); // { dg-error "cannot invoke derive macro" }
}
