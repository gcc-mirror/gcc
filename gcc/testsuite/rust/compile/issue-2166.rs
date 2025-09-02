#[lang = "sized"]
pub trait Sized {}

trait Add {
    type Output;

    fn add(self) -> u32;
}

impl Add for u32 {
    type Output = u32;

    fn add(self) -> u32 {
        0
    }
}

impl<'a> Add for &'a u32 {
    type Output = u32;

    fn add(self) -> <u32 as Add>::Output {
        0
    }
}
