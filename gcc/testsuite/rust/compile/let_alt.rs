fn main() {
    let _a | _a = 12;
    // { dg-error "top level or-patterns are not allowed for .let. bindings" "" { target *-*-* } .-1 }
    let (_b | _b) = 12;
}
