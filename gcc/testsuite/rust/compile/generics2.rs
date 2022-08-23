// { dg-error "expected .i32. got .i8." "" { target *-*-* } 0 }

struct GenericStruct<T>(T, usize);

fn main() {
    let a2: GenericStruct<i8>;
    a2 = GenericStruct(1, 456);

    let b2: i32 = a2.0;
    let c2: usize = a2.1;
}
