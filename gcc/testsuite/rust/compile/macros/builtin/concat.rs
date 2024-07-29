#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! concat {
    () => {{}};
}

fn main() {
    let not_literal = "identifier";
    concat!();
    concat! (,); // { dg-error "expected expression, found .,." }
    concat!(not_literal); // { dg-error "expected a literal" }
    concat!("message");
    concat!("message",);
    concat!("message", 1, true, false, 1.0, 10usize, 2000u64);
    concat!("message", 1, true, false, 1.0, 10usize, 2000u64,);
    concat! ("m", not_literal); // { dg-error "expected a literal" }
    concat!(not_literal invalid 'm' !!,); // { dg-error "expected token: .,." }
}
