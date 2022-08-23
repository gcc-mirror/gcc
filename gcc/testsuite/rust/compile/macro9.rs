macro_rules! add {
    ($e:expr, $($es:expr),*) => {
        $e + add!($($es),*)
    };
    ($e:expr) => {
        $e
    };
}

fn main() -> i32 {
    let a = add!(15 2 9); // { dg-error "Failed to match any rule within macro" }
    let b = add!(15);
    let b = add!(15 14); // { dg-error "Failed to match any rule within macro" }
    let b = add!(15, 14,); // { dg-error "Failed to match any rule within macro" }

    0
}
