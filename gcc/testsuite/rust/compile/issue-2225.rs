macro_rules! foo {
    ($_:tt) => {};
    () => {foo!($);};
}

macro_rules! bar {
    () => {let $_a = 12;} // { dg-error "unrecognised token" }
}

pub fn main() -> i32 {
    foo!();
    bar!();
    0
}
