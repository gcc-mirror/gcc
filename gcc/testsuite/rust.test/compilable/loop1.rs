fn main() {
    let mut a = 1;
    let mut b = 1;

    loop {
        let c = a + b;
        a = b;
        b = c;
    }
}
