#[lang = "sized"]
pub trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}
trait Tr {
    fn foo();

    fn bar(&self) {
        (|| self.foo())()
        // { dg-error "no method named .foo. found in the current scope .E0599." "" { target *-*-* } .-1 }
    }
}
