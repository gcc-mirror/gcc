// { dg-additional-options "-frust-compile-until=ast" }
//
// Do not reformat this test! The default rust format settings will insert a
// space between closure parameter lists.
fn main() {
    let f = |_||x, y| x+y;
    assert_eq!(f(())(1, 2), 3);
}
