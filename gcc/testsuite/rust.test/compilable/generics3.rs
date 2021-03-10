fn test<T>(a: T) -> T {
    a
}

fn main() {
    let a;
    a = test(123);
    let aa: i32 = a;

    let b;
    b = test::<u32>(456);
    let bb: u32 = b;
}
