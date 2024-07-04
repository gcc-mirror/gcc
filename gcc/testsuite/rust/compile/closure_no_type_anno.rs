#[lang = "sized"]
pub trait Sized {}

#[lang = "fn_once"]
trait FnOnce<Args> {
  type Output;

  fn call_once(self, args: Args) -> Self::Output;
}

pub fn foo() {
  let a = |_| 15; // { dg-error "type annotations needed" }
}
