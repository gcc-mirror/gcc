#[lang = "sized"]
trait Sized {}

trait Bar {}

struct S; // { dg-warning "struct is never constructed" }

pub fn test(foo: impl Bar) {}
