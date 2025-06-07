fn main() -> i32 {
    let mut x = 2;

    match x {
        a @ 2 => { x = a + 1 },
        _ => {}
    }

    x - 3
}
