// { dg-additional-options "-frust-compile-until=nameresolution" }
struct MyStruct;

trait X {}

fn test_a();
// { dg-error "free function without a body" "" { target *-*-* } .-1 }

impl MyStruct {
    fn test_b<T>()
    // { dg-error "associated function in .impl. without body" "" { target *-*-* } .-1 }
    where
        T: Copy;

    fn test_c<T>();
    // { dg-error "associated function in .impl. without body" "" { target *-*-* } .-1 }
}

impl X for MyStruct {
    fn test_d();
    // { dg-error "associated function in .impl. without body" "" { target *-*-* } .-1 }
}
