macro_rules! macro_rules {
    () => {
        struct S;
    };
}
macro_rules! {} // calls the macro defined above

fn main() {
    let _s = S;
}
