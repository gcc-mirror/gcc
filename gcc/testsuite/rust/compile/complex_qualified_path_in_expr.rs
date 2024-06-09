#[lang = "sized"]
pub trait Sized {}

struct S; // { dg-warning "struct is never constructed" }
impl S {
  fn f() -> i32 { return 0; }
  // { dg-warning "associated function is never used" "" { target *-*-* } .-1 }
}

trait T1 {
  type A;
  fn f() -> i32 { return 0; }
}

impl T1 for S {
  type A = S;
}

trait T2 {
  type B;
  fn f() -> i32 { return 0; }
}

impl T2 for S {
  type B = S;
}

trait T3 {
  fn f() -> i32 { return 0; }
}

impl T3 for S {}

fn main() {
  let _ = <<S as T1>::A as T2>::f();
  let _ = <<<S as T1>::A as T2>::B as T3>::f();
}
