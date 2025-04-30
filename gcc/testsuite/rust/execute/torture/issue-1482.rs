#[lang = "sized"]
trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

fn takes_fn(a: i32, f: impl FnOnce(i32) -> i32) -> i32 {
    f(a)
}

pub fn main() -> i32 {
    let capture = 2;
    let a = |i: i32| {
        let b = i + capture;
        b
    };
    takes_fn(1, a) - 3
}
