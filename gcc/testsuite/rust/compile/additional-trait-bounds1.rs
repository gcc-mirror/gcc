#![feature(optin_builtin_traits)]

pub unsafe auto trait Send {}
#[lang = "sync"]
pub unsafe auto trait Sync {}

trait A {}

impl dyn A + Send {}
impl dyn A + Send + Sync {}
