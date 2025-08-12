fn main() -> i32 {
    let foo @ (bar, _, _) = (0, 2, 3);
    let mut ret = 1;

    match foo {
        (0, 2, 3) => { ret = bar },
        _ => {}
    }

    ret
}