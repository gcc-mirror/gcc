// { dg-options "-w -O0 -fdump-tree-gimple" }
const A: usize = 123;
const B: [i32; 5] = [1, 2, 3, 4, 5];
const C: i32 = B[A];
// { dg-error "array subscript value .123. is outside the bounds of array" "" { target *-*-* } .-1 }
