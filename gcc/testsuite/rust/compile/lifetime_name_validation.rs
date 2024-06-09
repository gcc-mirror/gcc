pub fn one<'continue>() {} // { dg-error "lifetimes cannot use keyword names" }
pub fn two<'_>() {}

pub fn three<'static>() {}

pub fn four<'loop>() {} // { dg-error "lifetimes cannot use keyword names" }
