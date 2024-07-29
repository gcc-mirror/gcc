macro_rules! foo {
    () => {
        u32
    };
}

fn main() {
    let _a = 15i32;
    let _b = _a as foo!();
}
