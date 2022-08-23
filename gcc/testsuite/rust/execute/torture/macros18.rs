macro_rules! add {
    ($e:literal) => {
        0 + $e
    };
    ($e:literal $($es:literal)*) => {
        $e + add!($($es)*)
    };
}

fn main() -> i32 {
    let a = add!(3 4); // 7

    a - 7
}
