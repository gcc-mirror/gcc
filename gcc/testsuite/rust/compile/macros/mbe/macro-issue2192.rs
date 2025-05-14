macro_rules! foo {
    ($a:ident) => {}
}

pub fn bar() {
    foo!(self);
}
