fn test<T>(a: T) -> T {
    a
}

fn main() {
    let a: i32 = test(123);
    let b: i32 = test(456);
}
