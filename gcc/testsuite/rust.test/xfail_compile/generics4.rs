// { dg-excess-errors "Noisy error and debug" }
struct GenericStruct<T>(T, usize);

fn main() {
    let a2;
    a2 = GenericStruct::<i8, i32>(1, 456); // { dg-error "Invalid number of generic arguments to generic type" }

    let b2: i32 = a2.0;
    let c2: usize = a2.1;
}
