#[lang = "sized"]
pub trait Sized {}

struct Bidule<const N: i32 = 15> {}
enum Bidoule<const N: i32 = 15> {}

// Note - missing generic parameter - needs name resolution on const generics
type Bipboupe<const N: i32 = 15> = Bidule;
trait Fooable<const N: i32 = 15> {}

union Bidoulepe<const N: i32 = 15> {
    int: i32,
    float: f32,
}

fn const_default<const N: i32 = 15>() {} // { dg-error "default values for const generic parameters are not allowed here" }

// Note - missing generic parameter - needs name resolution on const generics
impl<const N: i32 = 15> Bidule {}
// { dg-error "default values for const generic parameters are not allowed here"   "" {target *-*-* } .-1 }
// { dg-error "unconstrained type parameter"  "" {target *-*-* } .-2 }
