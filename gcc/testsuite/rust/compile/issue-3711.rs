#[lang = "sized"]
pub trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

fn returns_closure() -> _ {
    // { dg-error "the type placeholder ._. is not allowed within types on item signatures .E0121." "" { target *-*-* } .-1 }
    || 0
}

fn main() {}
