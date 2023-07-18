#[lang = "sized"]
pub trait Sized {}

#[lang = "add"]
pub trait Add<RHS = Self> {
    type Output;

    fn add(self, rhs: RHS) -> Self::Output;
}

impl Add for u32 {
    type Output = u32;

    fn add(self, other: u32) -> u32 {
        self + other
    }
}

impl<'a> Add<u32> for &'a u32 {
    type Output = <u32 as Add<u32>>::Output;

    fn add(self, other: u32) -> <u32 as Add<u32>>::Output {
        Add::add(*self, other)
    }
}

impl<'a> Add<&'a u32> for u32 {
    type Output = <u32 as Add<u32>>::Output;

    fn add(self, other: &'a u32) -> <u32 as Add<u32>>::Output {
        Add::add(self, *other)
    }
}
