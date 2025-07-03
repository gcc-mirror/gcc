#[lang = "sized"]
trait Sized {}

pub enum ROption<T> {
    RSome(T),
    RNone,
}

fn main() {}
