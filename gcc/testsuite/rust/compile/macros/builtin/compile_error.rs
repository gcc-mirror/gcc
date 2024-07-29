#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! compile_error {
  () => {{}};
}

fn main () {
  let message = "error message";
  compile_error! (message); // { dg-error "argument must be a string literal" "" }
  compile_error! (); // { dg-error "macro takes 1 argument" "" }
  compile_error! ("a", "b"); // { dg-error "macro takes 1 argument" "" }
  compile_error! ("expected error message"); // { dg-error "expected error message" }
  compile_error! ("expected error message",); // { dg-error "expected error message" }
}
