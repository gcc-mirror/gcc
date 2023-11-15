#[lang = "sized"]
pub trait Sized {}
trait Trait: ?Sized {} // { dg-error ".?Trait. is not permitted in supertraits" }
