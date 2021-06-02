fn main() {
    let mut a = 1;
    let mut b = 1;

    // first number in Fibonacci sequence over 10:
    loop {
        if b > 10 {
            break;
        }
        let c = a + b;
        a = b;
        b = c;
    }
}
