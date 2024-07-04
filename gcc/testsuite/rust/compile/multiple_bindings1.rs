fn f1(i: i32, i: i32) {}
// { dg-error "identifier .i. is bound more than once in the same parameter list .E0415." "" { target *-*-* } .-1 }

trait Foo {
  fn f2(i: i32, i: i32) {}
  // { dg-error "identifier .i. is bound more than once in the same parameter list .E0415." "" { target *-*-* } .-1 }
}

trait Bar {
  fn f3(i: i32, j: i32) {}
}

struct S;

impl S {
  fn f4(i: i32, i: i32) {}
  // { dg-error "identifier .i. is bound more than once in the same parameter list .E0415." "" { target *-*-* } .-1 }
}

impl Bar for S {
  fn f3(i: i32, i: i32) {}
  // { dg-error "identifier .i. is bound more than once in the same parameter list .E0415." "" { target *-*-* } .-1 }
}

fn main() {
  let _ = |i, i| {};
  // { dg-error "identifier .i. is bound more than once in the same parameter list .E0415." "" { target *-*-* } .-1 }
}

