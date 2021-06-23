pub fn main() {
    fn test<T>(x: T) -> T {
        x
    }

    let mut a = 123;
    a = test(a);

    let mut b = 456f32;
    b = test(b);
}
