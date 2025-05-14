#[lang = "sized"]
pub trait Sized {}

#[lang = "add"]
pub trait Add<Rhs = Self> {
    type Output;

    fn add(self, rhs: Rhs) -> Self::Output;
}
