#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! env {
  () => {{}};
}

fn main () {
  let message = "error message";
  env! (message); // { dg-error "argument must be a string literal" "" }
  env! (); // { dg-error "env! takes 1 or 2 arguments" "" }
  env! (,); // { dg-error "expected expression, found .,." "" }
  env! (1); // { dg-error "argument must be a string literal" "" }
  env! ("NOT_DEFINED"); // { dg-error "environment variable 'NOT_DEFINED' not defined" "" }
  env! ("NOT_DEFINED",); // { dg-error "environment variable 'NOT_DEFINED' not defined" "" }
  env! ("NOT_DEFINED", 1); // { dg-error "argument must be a string literal" "" }
  env! ("NOT_DEFINED", "two", "three"); // { dg-error "env! takes 1 or 2 arguments" "" }
  env! ("NOT_DEFINED" "expected error message"); // { dg-error "expected token: .,." "" }
  env! ("NOT_DEFINED", "expected error message"); // { dg-error "expected error message" "" }
  env! ("NOT_DEFINED", "expected error message",); // { dg-error "expected error message" "" }
  env! (1, "two"); // { dg-error "argument must be a string literal" "" }
}
