pub fn main() {
    let a = 123;

    fn test(x: i32) -> i32 {
        x + 456
    }

    let b;
    b = test(a);
}
