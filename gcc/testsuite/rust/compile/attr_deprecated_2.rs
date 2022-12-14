#[deprecated(since="1.0")]
fn test1() {}

// { dg-excess-errors "unknown meta item ...." }
#[deprecated(invalid="invalid")]
fn test2() {}

fn main() {
    test1(); // { dg-warning ".attr_deprecated_2::test1. is deprecated" }
    test2();
}
