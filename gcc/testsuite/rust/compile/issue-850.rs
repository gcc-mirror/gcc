#[lang = "sized"]
pub trait Sized {}

extern "C" {
    fn printf(s: *const i8, ...);
}

mod option {
    enum Option<T> {
        #[lang = "None"]
        None,
        #[lang = "Some"]
        Some(T),
    }
}

pub use option::Option::{self, None, Some};

fn divide(numerator: f64, denominator: f64) -> Option<f64> {
    if denominator == 0.0 {
        None
    } else {
        Some(numerator / denominator)
    }
}

fn main() {
    let result = divide(2.0, 3.0);

    match result {
        Some(x) => unsafe {
            let a = "Result: %i\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c, x);
        },
        None => unsafe {
            let a = "Cannot divide by 0\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        },
    }
}
