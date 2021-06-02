fn main() {
    let mut a = 1;
    let mut b = 1;

    // first number in Fibonacci sequence over 10:
    let _fib = loop {
        if b > 10 {
            break b;
        }
        let c = a + b;
        a = b;
        b = c;
    };
}
