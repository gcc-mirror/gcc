fn foo (&a: &i32, b: i32) -> i32 {
  a + b
}

fn main() -> i32 {
  let a = 4;
  foo(&a, 2) - 6
}