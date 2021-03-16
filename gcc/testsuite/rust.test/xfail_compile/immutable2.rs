const TEST_CONST: i32 = 10;

fn main() {
    TEST_CONST = 1; // { dg-error "cannot assign to immutable" }
}
