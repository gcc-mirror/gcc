pub use result::Result::{self, Err, Ok};

#[lang = "sized"]
pub trait Sized {}

extern "C" {
    fn printf(s: *const i8, ...);
}

mod result {
    pub enum Result<T, E> {
        #[lang = "Ok"]
        Ok(T),

        #[lang = "Err"]
        Err(E),
    }
}

pub fn test(a: i32) -> Result<i32, bool> {
    if a > 5 {
        Ok(123)
    } else {
        Err(false)
    }
}
