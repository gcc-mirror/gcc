// { dg-additional-options "-frust-compile-until=typecheck" }

fn function_pointer_as_argument(f: for<'a> fn(&'a i32) -> &'a i32) -> i32 {
    0
}

fn function_pointer_as_return() -> for<'a> fn(&'a i32) -> &'a i32 {
}

// https://doc.rust-lang.org/reference/trait-bounds.html

trait Fn<T> {}
fn call_on_ref_zero<F>(f: F) where for<'a> F: Fn(&'a i32) {}

fn call_on_ref_zero2<F>(f: F) where F: for<'a> Fn(&'a i32) {}

trait Trait<'a, T: 'a> {}

impl<'a, T> Trait<'a, T> for &'a T {}