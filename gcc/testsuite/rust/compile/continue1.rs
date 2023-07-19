fn main() {
    let mut a = 1;
    let mut b = 1;

    let _fib = {
        continue;   // { dg-error ".continue. outside of a loop" }
        123
    };
}
