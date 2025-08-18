// { dg-additional-options "-frust-compile-until=typecheck" }
// TODO: this should typecheck

#[lang = "sized"]
trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    /// The returned type after the call operator is used.
    #[lang = "fn_once_output"]
    type Output;

    /// Performs the call operation.
    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

pub fn foo() {
    (|_a @ _b| {}) (1)
}
