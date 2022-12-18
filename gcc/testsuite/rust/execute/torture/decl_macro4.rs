#![feature(decl_macro)]
pub macro add {
    ($e:expr) => {
        $e
    },
    ($h:expr, $($t:expr),*) => {
        $h + add!($($t),*)
    },
}

fn main() -> i32 {
    let a = add!(1, 2, 3);

    a - 6
}
