#![feature(no_core, lang_items)]
#![no_core]
#[lang = "sized"]
pub trait Sized {}
#[lang = "fn_once"]
trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;
    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}
trait Iterator {
    type Item;
}
pub struct OnceWith<F> {
    pub f: F,
}
impl<A, F: FnOnce() -> A> Iterator for OnceWith<F> {
    type Item = A;
}
