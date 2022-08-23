#[deprecated(since="1.0", note="do not use this function")]
fn test1() {}

#[deprecated]
fn test() {}

#[deprecated = "a different message"]
fn test2() {}

fn main() {
    test(); // { dg-warning ".attr_deprecated::test. is deprecated" }
    test1(); // { dg-warning ".attr_deprecated::test1. is deprecated: do not use this function" }
    test2(); // { dg-warning ".attr_deprecated::test2. is deprecated: a different message" }
}
