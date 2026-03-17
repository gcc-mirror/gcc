#![feature(no_core)]
#![no_core]

#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

#[lang = "fn_once"]
trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

macro_rules! closure {
    () => {{
        14 + 15
    }};
}

fn main() {
    let _ = || closure!();
}
