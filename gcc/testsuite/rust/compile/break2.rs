fn main() {
    let mut a = 1;
    let mut b = 1;

    let mut c;
    while b > 10 {
        if (b == 2) {
            break b;  // { dg-error "can only break with a value inside 'loop'" }
            // { dg-error "failed to type resolve expression" "" { target *-*-* } .-1 }
        }
        c = a + b;
        a = b;
        b = c;
    }
}
