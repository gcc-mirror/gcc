struct GenericStruct<T>(T, usize);

impl<T> GenericStruct<T> {
    fn new(a: T, b: usize) -> Self {
        GenericStruct(a, b)
    }

    fn get(self) -> T {
        self.0
    }
}

fn main() {
    let a: GenericStruct<i32> = GenericStruct::<i32>::new(123, 456);
    let aa: i32 = a.get();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let b: GenericStruct<u32> = GenericStruct::<_>::new(123, 456);
    let bb: u32 = b.get();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let c: GenericStruct<f32> = GenericStruct::new(123f32, 456);
    let cc: f32 = c.get();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
