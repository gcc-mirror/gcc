#[lang = "sized"]
pub trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

fn main() -> i32 {
  let foo = |&&d: &&i32| -> i32 { d };

  let x = &&5i32;
  foo(x) - 5
}
