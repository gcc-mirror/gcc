const TEST: usize = 6;

fn main() {
    let a: [_; 12] = [123; TEST * 2];
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
