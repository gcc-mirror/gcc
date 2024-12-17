// { dg-additional-options "-frust-name-resolution-2.0" }
#[lang = "sized"]
trait Sized {}

pub enum ROption<T> {
    RSome(T),
    RNone,
}

fn main() {}
