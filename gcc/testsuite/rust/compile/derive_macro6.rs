#[lang = "sized"]
pub trait Sized {}

#[lang = "copy"]
pub trait Copy {}

#[lang = "clone"]
pub trait Clone {
    fn clone(&self) -> Self;
}

#[lang = "phantom_data"]
pub struct PhantomData<T>;

impl Copy for i32 {}
impl Copy for i64 {}
impl Copy for U {}

#[derive(Clone)]
union U {
    i: i32,
    f: f64,
}
