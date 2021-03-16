fn test1() -> i32 {
    return 2;
    1
}

fn test2(x: i32) -> i32 {
    if x > 1 {
        return 5;
    } else {
        return 0;
    }
    return 1;
}

fn main() {
    let call1 = test1();
    let call2 = test2(2);
}
