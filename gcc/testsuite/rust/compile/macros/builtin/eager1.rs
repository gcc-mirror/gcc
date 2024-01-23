// { dg-additional-options "-fdump-tree-gimple" }
#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! concat {
    () => {{}};
}

macro_rules! a {
    () => { "test" };
}

macro_rules! b {
    () => { "canary" };
}

fn main() {
    // { dg-final { scan-assembler {"test1canary"} } }
    let _ = concat!(a!(), 1, b!());
    // should not error
    concat!(a!(), true, b!(),);
}
