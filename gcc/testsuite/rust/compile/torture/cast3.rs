fn main() {
    let a = "foo\0";
    let b = a as *const str;
    let c = b as *const i8;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
