const FOO: i32 = if true { [1, 2, 3] } else { [2, 3, 4] }[0];

pub fn test() -> i32 {
    FOO
}
