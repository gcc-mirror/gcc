#[lang = "sized"]
pub trait Sized {}

extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

fn main() -> i32 {
    let closure_annotated = |i: i32| -> i32 { i + 1 };

    let i = 1;
    closure_annotated(i) - 2
}
