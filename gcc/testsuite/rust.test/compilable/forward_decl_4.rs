fn main() {
    let mut x = TEST_CONST;
    x = x + 1;

    let mut y = x + TEST_CONST;
}

const TEST_CONST: i32 = 10;
