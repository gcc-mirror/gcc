macro_rules! add {
    ($a:expr,$b:expr) => {
        $a + $b
    };
}

fn test() -> i32 {
    add!(1 + 2, 3)
}

fn main() -> i32 {
    test() - 6
}
