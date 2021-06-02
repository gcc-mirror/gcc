const TEST_CONST: i32 = 10;

fn test(x: u32) -> u32 {
    x + 1
}

fn main() {
    let x = TEST_CONST;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let a = 1u32;
    let b = a;

    let c;
    c = 1;

    let d;
    d = b;

    let param;
    param = 123;

    let test_call = test(param);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
