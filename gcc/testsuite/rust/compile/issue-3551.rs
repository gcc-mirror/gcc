#[lang = "sized"]
pub trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

struct Bug {
    a: [(); (|| 0)()],
    // { dg-error "calls in constants are limited to constant functions, tuple structs and tuple variants" "" { target *-*-* } .-1 }
}
