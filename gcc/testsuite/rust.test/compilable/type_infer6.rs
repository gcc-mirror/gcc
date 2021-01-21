fn test(x: u32) -> u32 {
    return x + 1;
}

fn main() {
    let a;
    a = 1;
    let b = test(a);

    let c = 1;
    let d = test(c + 1);
}
