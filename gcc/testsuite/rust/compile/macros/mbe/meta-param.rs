macro_rules! foo {
    ($x:meta) => {0}
}

pub fn main() -> i32 {
    foo!(Clone)
}
