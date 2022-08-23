fn main() {
    let mut a = 1;
    let mut b = 1;

    let _fib = {
        continue; // { dg-error "cannot 'continue' outside of a loop" }
        // { dg-error "failed to type resolve expression" "" { target *-*-* } .-1 }
        123
    };
}
