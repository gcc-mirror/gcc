pub fn test() -> i32 {
    let a = unsafe { 123 };
    a
}

pub fn main() {
    let a = test();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
