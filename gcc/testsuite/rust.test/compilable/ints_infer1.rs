const TEST_CONST: i32 = 10;

fn main() {
    let mut x = TEST_CONST;
    x = x + 1;

    let mut y = x + TEST_CONST;

    let z = 1u32;

    let a = z;

    let b;
    b = 1;

    let c;
    c = a;
}
