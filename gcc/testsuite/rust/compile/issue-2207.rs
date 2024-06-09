macro_rules! finish {
    (+ - + * + /) => {}
}

macro_rules! foo {
    () => { foo!(+ - * /); };
    ($a:tt $($b:tt)*) => { finish!($($a $b)*); }
}

pub fn bar() {
    foo!();
}
