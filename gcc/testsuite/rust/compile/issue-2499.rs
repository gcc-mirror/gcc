#[lang = "sized"]
pub trait Sized {}

struct Foo;
struct Bar;

impl Foo for Bar {}
// { dg-error "Expected a trait found .Foo. .E0404." "" { target *-*-* } .-1 }

fn baz<T: Foo>(t: T) {}
// { dg-error "Expected a trait found .Foo. .E0404." "" { target *-*-* } .-1 }
