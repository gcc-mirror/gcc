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
    // { dg-final { scan-tree-dump-times {"test1canary"} 1 gimple } }
    let _ = concat!(a!(), 1, b!());
    // should not error
    concat!(a!(), true, b!(),);
}
