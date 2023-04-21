fn main() {
    let _a | _a = 12;
    // { dg-error "top level alternate patterns are not allowed" "" { target *-*-* } .-1 }
}
