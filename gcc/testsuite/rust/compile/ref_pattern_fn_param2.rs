fn foo(&b: &i32) -> bool {
  b == 0
}

fn main() {
  let _ = foo(&0);
}
