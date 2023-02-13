extern "C" {
    fn printf(s: *const i8, ...);
}

fn main() {
    // { dg-error "expected .c_int. variadic argument" "" { target *-*-* } .+1 }
    printf("%d\n" as *const str as *const i8, 1i8);
}
