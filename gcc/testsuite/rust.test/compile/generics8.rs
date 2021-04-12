struct GenericStruct<T>(T, usize);

impl<T> GenericStruct<T> {
    fn new(a: T, b: usize) -> Self {
        GenericStruct(a, b)
    }
}

fn main() {
    let a: GenericStruct<i32> = GenericStruct::<i32>::new(123, 456);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let b: GenericStruct<u32> = GenericStruct::<_>::new(123, 456);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let c: GenericStruct<f32> = GenericStruct::new(123f32, 456);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
