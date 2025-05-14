#[lang = "sized"]
pub trait Sized {}

#[lang = "copy"]
pub trait Copy {}

#[lang = "clone"]
pub trait Clone {
    fn clone(&self) -> Self;
}

#[lang = "phantom_data"]
struct PhantomData<T>;

#[derive(Clone)] // { dg-error "bounds not satisfied for U .Copy. is not satisfied" }
union U {
    i: i32,
    f: f64,
}
