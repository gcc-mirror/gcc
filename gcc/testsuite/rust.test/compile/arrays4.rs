const TEST: usize = 4;

fn main() {
    let a: [_; TEST + 1 + 2] = [123; 7];
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
