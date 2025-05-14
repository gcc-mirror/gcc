#[lang="sized"]
trait Sized {}


// E0044
fn main() {
extern "C" { fn some_func<T>(x: T); } // { dg-error "foreign items may not have type parameters .E0044." }
}