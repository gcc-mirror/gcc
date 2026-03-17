#![feature(no_core)]
#![no_core]

fn bar() {
    foo();

    fn foo() {
        fn bar2() {
            foo();
        }

        bar2();
    }
}

fn main() {
    bar();
}
