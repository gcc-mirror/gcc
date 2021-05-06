// Current errors are too noisy to match specific ones.
// { dg-error "failed to resolve TypePath: T" "" { target *-*-* } 0 }
// { dg-error "unresolved type" "" { target *-*-* } 0 }

struct GenericStruct<T>(T, usize);

fn main() {
    let a2;
    a2 = GenericStruct::<i8, T>(1, 456);

    let b2: i32 = a2.0;
    let c2: usize = a2.1;
}
