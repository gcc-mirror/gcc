extern "C" {
    fn printf(s: *const i8, ...);
}

fn main() {
    // { dg-error "expected .c_int. variadic argument" "" { target *-*-* } .+1 }
    printf("%d\n" as *const str as *const i8, 1i8); 

    // { dg-error "expected .c_uint. variadic argument" "" { target *-*-* } .+1 }
    printf("%d\n" as *const str as *const i8, 1u8); 

     // { dg-error "expected .c_double. variadic argument" "" { target *-*-* } .+1 }
    printf("%d\n" as *const str as *const i8, 1f32);
}
