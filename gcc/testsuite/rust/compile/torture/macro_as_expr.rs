// { dg-additional-options "-w" }

macro_rules! add {
    ($a:expr) => { $a };
    ($a:expr, $($b:expr),+) => { $a + add!($($b),*) }
}

fn main() -> i32 {
    if add!(add!(1, 2)) > add!(5) {
        add!(1, add!(2, 3), add!(4))
    } else {
        add!(5, add!(6, 7), add!(8), 9) + 10
    }
}
