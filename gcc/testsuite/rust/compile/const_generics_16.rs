#[lang = "sized"]
trait Sized {}

struct Foo<T = u8, const N: usize = 4> {
    data: [T; N], // { dg-warning "field is never read: .data." }
}

fn main() {
    let _x = Foo { data: [1, 2, 3, 4] };
}
