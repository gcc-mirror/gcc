const SIZE: usize = 14 + 2;
const TEST: [i32; SIZE] = [2; SIZE];
// { dg-warning "unused name" "" { target *-*-* } .-1 }
