#![feature(optin_builtin_traits)]

pub unsafe auto trait Send {}
#[lang = "sync"]
pub unsafe auto trait Sync {}

trait A {}

impl dyn A + Send + Sync + NonExist {} // { dg-error "failed to resolve TypePath: NonExist in this scope" }
