// { dg-additional-options "-frust-compile-until=ast" }
fn main() {
    only_foo::<<i32 as Bar>::Item>();
}
