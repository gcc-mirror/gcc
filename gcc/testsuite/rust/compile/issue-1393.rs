fn tst() {
    let a = 123;
    let b = 0;
    let _c = if b == 0 {
        (a & 0x7fffff) << 1
    } else {
        (a & 0x7fffff) | 0x800000
    };
}

fn main() {
    tst()
}
