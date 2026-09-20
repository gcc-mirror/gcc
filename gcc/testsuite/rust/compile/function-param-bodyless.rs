// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

mod extern_test {
    extern "C" {
        // { dg-error "pattern not allowed in foreign function .E0130." "" { target *-*-* } .+1 }
        fn f1(mut a: i32);
        // { dg-error "pattern not allowed in foreign function .E0130." "" { target *-*-* } .+1 }
        fn f2(ref a: i32);
        // { dg-error "pattern not allowed in foreign function .E0130." "" { target *-*-* } .+1 }
        fn f3(a @ _: i32);
        // { dg-error "pattern not allowed in foreign function .E0130." "" { target *-*-* } .+1 }
        fn f4((a,): i32);
        // { dg-error "pattern not allowed in foreign function .E0130." "" { target *-*-* } .+1 }
        fn f5((a): i32);

        fn f6(_: i32);
    }
}

trait Foo {
    // { dg-error "pattern not allowed in function without body .E0642." "" { target *-*-* } .+1 }
    fn f1(mut a: i32);
    // { dg-error "pattern not allowed in function without body .E0642." "" { target *-*-* } .+1 }
    fn f2(ref a: i32);
    // { dg-error "pattern not allowed in function without body .E0642." "" { target *-*-* } .+1 }
    fn f3(a @ _: i32);
    // { dg-error "pattern not allowed in function without body .E0642." "" { target *-*-* } .+1 }
    fn f4((a,): i32);
    // { dg-error "pattern not allowed in function without body .E0642." "" { target *-*-* } .+1 }
    fn f5((a): i32);

    fn f6(_: i32);
}
