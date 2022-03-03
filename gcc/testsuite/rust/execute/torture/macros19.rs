macro_rules! add {
    ($e:expr, $($es:expr),*) => {
        $e + add!($($es),*)
    };
    ($e:expr) => {
        $e
    };
}

fn main() -> i32 {
    let a = add!(15, 2, 9); // 26

    a - 26
}
