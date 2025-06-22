fn main() -> i32 {
    struct A (i32, i32);
    let a = A (0, 1);
    let mut ret = 1;

    match a {
        A (0, b) => { ret -= b },
        _ => {}
    }

    ret
}
