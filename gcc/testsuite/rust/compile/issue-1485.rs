#[lang = "sized"]
pub trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

struct BinOpInvalid {
    lhs: i32,
    rhs: i32,
    f: impl FnOnce(i32) -> i32, // { dg-error ".impl Trait. not allowed outside of function and inherent method return types .E0562." }
}
