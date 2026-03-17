// Check the follow-set of :vis in macro rules.
#![feature(no_core)]
#![no_core]


macro_rules! my_mac {
    ($v:vis async) => {
        $v struct Foo(i32);
    };
    ($v:vis $i:ident) => {
        $v struct $i(i32);
    }
}
