// Test for issue #4213 - rogue macro detected during Lowering.

macro_rules! inner {
    () => {
        $crate::    // { dg-error "could not parse path expression segment" }
    };
}

macro_rules! multi_arm {
    (a) => { $crate:: };     // { dg-error "could not parse path expression segment" }
    (b) => { () };
}

macro_rules! another_macro {
    () => { $crate:: }      // { dg-error "could not parse path expression segment" }
}
macro_rules! generic_macro {
    ($t:ty) => {
        $crate::            // { dg-error "could not parse path expression segment" }
    };
}

macro_rules! empty_case {
    () => {};
}

pub fn main() {
    let _ = inner!();
    let _ = multi_arm!(a);
    let _ = multi_arm!(b);
    let _ = another_macro!();
    let _ = generic_macro!(i32);
    empty_case!();
}