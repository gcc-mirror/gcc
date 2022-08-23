fn test(a: u32) -> u32 {
    a + 1
}

fn main() {
    let param;
    param = 123;

    let a = test(param);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
