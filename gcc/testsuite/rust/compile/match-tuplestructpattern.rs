fn main() {
    struct A (i32, i32);
    let a = A (0, 1);

    match a {
        A (0, 1) => {},
        _ => {}
    }
}
