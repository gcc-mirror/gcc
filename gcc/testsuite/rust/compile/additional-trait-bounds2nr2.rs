// { dg-additional-options "-frust-name-resolution-2.0" }

#![feature(optin_builtin_traits)]

pub unsafe auto trait Send {}
#[lang = "sync"]
pub unsafe auto trait Sync {}

trait A {}

impl dyn A + Send + Sync + NonExist {} // { dg-error "could not resolve type path .NonExist." }
