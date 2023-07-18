#[lang = "sized"]
pub trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

fn main() {
    let lambda = |&c| c != b'9';

    let a = b'1';
    lambda(&a);
}
