#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! crabby_crab_carb { // { dg-error "cannot find a built-in macro with name .crabby_crab_carb." }
    () => {{}};
}
