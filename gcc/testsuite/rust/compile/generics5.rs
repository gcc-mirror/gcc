struct GenericStruct<T>(T, usize);

fn main() {
    let a2;
    a2 = GenericStruct::<i8, T>(1, 456);
    // { dg-error "could not resolve type path .T." "" { target *-*-* } .-1 }

    let b2: i32 = a2.0;
    let c2: usize = a2.1;
}
