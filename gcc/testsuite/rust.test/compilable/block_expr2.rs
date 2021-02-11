fn test() -> i32 {
    123
}

fn main() {
    let a = { test() };
    let b = {
        if a > 10 {
            a - 1
        } else {
            a + 1
        }
    };
}
