fn foo(state: &mut [u32; 16], a: usize) {
    // { dg-warning "function is never used: .foo." "" { target *-*-* } .-1 }
    state[a] = 1;
}
