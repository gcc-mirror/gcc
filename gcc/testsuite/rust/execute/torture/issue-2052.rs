#[lang = "sized"]
pub trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

pub fn f() -> i32 {
    (|| 42)()
}

pub fn main() -> i32 {
    f() - 42
}
