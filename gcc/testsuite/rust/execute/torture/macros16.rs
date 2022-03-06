macro_rules! add {
    ($e:literal) => {
        0 + $e
    };
    ($e:literal $($es:literal)*) => {
        $e + add!($($es)*)
    };
}

fn main() -> i32 {
    let a = add!(1 2 3 10); // 16

    a - 16
}
