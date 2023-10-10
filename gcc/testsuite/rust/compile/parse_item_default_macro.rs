// { dg-additional-options "-frust-compile-until=ast" }
macro_rules! default {
    ($($x:tt)*) => { $($x)* }
}

default! {
    struct A;
}
