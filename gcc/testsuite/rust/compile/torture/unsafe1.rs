fn test() -> i32 {
    unsafe {
        let a;
        a = 123;
        a
    }
}

fn main() {
    let a;
    a = test();
}
