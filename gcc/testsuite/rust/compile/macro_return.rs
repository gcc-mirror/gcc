// { dg-additional-options "-w" }

macro_rules! add {
    ($a:expr) => { $a };
    ($a:expr, $($b:expr),+) => { $a + add!($($b),*) }
}

fn main() -> i32 {
    add!(add!(1, 2))
}
