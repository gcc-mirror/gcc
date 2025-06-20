#[lang = "sized"]
trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
    type Output;
}
