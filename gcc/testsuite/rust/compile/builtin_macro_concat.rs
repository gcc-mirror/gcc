macro_rules! concat {
    () => {{}};
}

fn main() {
    // let not_literal = "identifier";
    concat!();
    // concat! (,); // { error "argument must be a constant literal" }
    // concat!(not_literal); // { error "argument must be a constant literal" }
    concat!("message");
    concat!("message",);
    concat!("message", 1, true, false, 1.0, 10usize, 2000u64);
    concat!("message", 1, true, false, 1.0, 10usize, 2000u64,);
    // concat! ("m", not_literal); // { error "argument must be a constant literal" }
}
