fn main() {
    let a: *const i32 = &123;
    let b: *mut i32 = (a as *mut i32);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
