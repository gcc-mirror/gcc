fn main() {
    let mut a = 1;
    let mut b = 1;

    let _fib = loop {
        // { dg-bogus "unused name" "#361" { xfail *-*-* } .-1 }
        if (a % 2 == 0) {
            continue;
        }
        let c = a + b;
        a = b;
        b = c;
    };
}
