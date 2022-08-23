/// doc comment 1
/// doc comment 2
/// `blah blah` markdown
pub struct TestStruct {}

#[doc(hidden)]
pub struct DocAttribute {}

#[doc(a,b)]
pub struct UnkAttribute {}

fn main() {
    let _ = TestStruct {};
    let _ = DocAttribute {};
    let _ = UnkAttribute {};
}
