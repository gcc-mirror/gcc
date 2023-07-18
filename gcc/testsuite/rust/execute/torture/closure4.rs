#[lang = "sized"]
pub trait Sized {}

#[lang = "fn_once"]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

struct Foo(i32, i32);

fn main() -> i32 {
  let foo = |&x: &i32, y: i32| -> i32 {
    x + y
  };
  
  let bar = |Foo(x, y): Foo| -> i32 {
    x + y
  };

  let a = 4;
  foo(&a, 2) + bar(Foo(100, 200)) - 306
}
