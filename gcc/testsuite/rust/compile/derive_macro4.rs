#[lang = "sized"]
pub trait Sized {}

pub trait Copy {}
pub trait Clone {
    fn clone(&self) -> Self;
}

struct PhantomData<T>;

pub struct AssertParamIsCopy<T: Copy> {
    _field: PhantomData<T>,
}

#[derive(Clone)] // { dg-error "bounds not satisfied for U .Copy. is not satisfied" }
union U {
    i: i32,
    f: f64,
}
