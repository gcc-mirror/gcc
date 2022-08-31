// { dg-additional-options "-frust-compile-until=unsafety" }

unsafe fn foo() {}

fn main() {
    foo()
}
