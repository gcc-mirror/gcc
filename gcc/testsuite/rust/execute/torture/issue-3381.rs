/* { dg-output "Err: 15\r*\n" } */
#[lang = "sized"]
trait Sized {}

enum Result<T, E> {
    #[lang = "Ok"]
    Ok(T),
    #[lang = "Err"]
    Err(E),
}

#[lang = "try"]
pub trait Try {
    type Ok;
    type Error;

    #[lang = "into_result"]
    #[unstable(feature = "try_trait", issue = "42327")]
    fn into_result(self) -> Result<Self::Ok, Self::Error>;

    #[lang = "from_error"]
    #[unstable(feature = "try_trait", issue = "42327")]
    fn from_error(v: Self::Ok) -> Self;

    #[lang = "from_ok"]
    #[unstable(feature = "try_trait", issue = "42327")]
    fn from_ok(v: Self::Error) -> Self;
}

impl<T, E> Try for Result<T, E> {
    type Ok = T;
    type Error = E;

    fn into_result(self) -> Result<T, E> {
        self
    }

    fn from_ok(v: T) -> Self {
        Result::Ok(v)
    }

    fn from_error(v: E) -> Self {
        Result::Err(v)
    }
}

pub trait From<T>: Sized {
    fn from(_: T) -> Self;
}

impl<T> From<T> for T {
    fn from(t: T) -> Self {
        t
    }
}

fn print(s: &str, value: i32) {
    extern "C" {
        fn printf(s: *const i8, ...);
    }

    unsafe {
        printf(s as *const str as *const i8, value);
    }
}

fn baz() -> Result<i32, i32> {
    Result::Err(15)
}

fn foo() -> Result<i32, i32> {
    let b = match baz() {
        Result::Ok(value) => value,
        Result::Err(err) => {
            return Try::from_error(From::from(err));
        }
    };

    Result::Ok(15 + b)
}

fn main() -> i32 {
    let a = foo();
    match a {
        Result::Ok(value) => print("Ok: %i\n", value),
        Result::Err(err) => print("Err: %i\n", err),
    };

    0
}
