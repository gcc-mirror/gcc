#[lang = "sized"]
pub trait Sized {}

fn test<T>(a: T) -> T {
  a
}

fn main() {
  let t: i32 = test(123 as i32) as i32;
  // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
