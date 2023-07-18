#[lang = "sized"]
pub trait Sized {}

struct GenericStruct<T>(T, usize);

impl GenericStruct<i32> {
    fn new(a: i32, b: usize) -> Self {
        GenericStruct(a, b)
    }

    fn get(self) -> i32 {
        self.0
    }
}

fn main() {
    let a: GenericStruct<i32> = GenericStruct::<i32>::new(123, 456);
    let aa: i32 = a.get();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
