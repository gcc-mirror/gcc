// { dg-options "-w" }
#[lang = "sized"]
pub trait Sized {}

#[lang = "phantom_data"]
struct PhantomData<T>;

trait Hash {
    fn hash<H>(&self, state: &mut H);
}

impl<T> Hash for PhantomData<T> {
    fn hash<H>(&self, state: &mut H) {}
}
