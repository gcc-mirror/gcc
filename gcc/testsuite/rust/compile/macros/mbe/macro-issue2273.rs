macro_rules! mac {
    () => {();} // { dg-warning "trailing semicolon" }
}

pub fn foo() {
    mac!()
}
