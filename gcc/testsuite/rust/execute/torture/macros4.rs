macro_rules! add {
    ($a:expr,$b:expr) => {
        $a + $b
    };
    ($a:expr) => {
        $a
    };
}

fn main() -> i32 {
    let mut x = add!(1);
    x += add!(2, 3);

    x - 6
}
