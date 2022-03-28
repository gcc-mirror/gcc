macro_rules! t {
    () => {
        i32
    };
}

fn id<T>(arg: T) -> T {
    arg
}

fn main() -> i32 {
    id::<t!()>(15) - 15
}
