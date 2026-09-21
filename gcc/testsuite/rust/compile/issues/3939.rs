#![feature(no_core)]
#![feature(lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

#[lang = "copy"]
trait Copy {}

#[lang = "fn_once"]
trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    fn call_once(self, args: Args) -> Self::Output;
}

#[lang = "fn_mut"]
trait FnMut<Args>: FnOnce<Args> {
    fn call_mut(&mut self, args: Args) -> Self::Output;
}

#[lang = "fn"]
trait Fn<Args>: FnMut<Args> {
    fn call(&self, args: Args) -> Self::Output;
}

#[derive(Copy)]
pub struct X<T, F: Fn(T) -> T>(T, F);
