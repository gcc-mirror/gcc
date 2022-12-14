fn bar (x: u8, y: u8) -> i32 {
    match (x, y) {
        (1, 1) => { return 1; }
        (1, _) => { return -1; }
    }

    return 0;
}

fn main () -> () {
    bar (1, 2);
}
