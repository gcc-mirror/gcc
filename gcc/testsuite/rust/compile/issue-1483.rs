#[lang = "sized"]
pub trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

pub fn takes_fn_generic<F: FnOnce(i32) -> i32>(a: i32, f: F) -> i32 {
    f(a)
}

pub fn takes_fn_generic_where<F>(a: i32, f: F) -> i32
where
    F: FnOnce(i32) -> i32,
{
    f(a)
}

pub fn test() {
    let foo = |x: i32| -> i32 { x + 1 };

    takes_fn_generic(1, foo);
    takes_fn_generic_where(2, foo);
}
