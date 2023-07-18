#[lang = "sized"]
pub trait Sized {}

macro_rules! foo {
    () => {"foo"};
    (number) => { 12 };
    (false) => { false };
}

pub const A: &'static str = foo!();
pub static B: &'static str = foo!();

pub trait Number {
    const VALUE: u32;
}

impl Number for u32 {
    const VALUE: u32 = foo!(number);
}

impl u32 {
    pub const TWELVE: u32 = foo!(number);
}

pub enum E {
    Variant = foo!(number),
}

pub fn f(c: bool) -> &'static str {
    match c {
        false => foo!(),
        true if foo!(false) => "abc",
        _ => "xyz"
    }
}


fn main() {
    let _ = A;
    let _ = u32::VALUE - u32::TWELVE;
}
