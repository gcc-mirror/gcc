const fn add(x: usize, y: usize) -> i32 {
    add + y
    // { dg-error "cannot apply operator .+. to types fn .x usize,y usize,. -> i32 and usize" "" { target *-*-* } .-1 }
}
const ARR: [i32; add(1, 2)] = [5, 6, 1];
// { dg-error "mismatched types, expected .usize. but got .i32. .E0308." "" { target *-*-* } .-1 }
// { dg-error "mismatched types" "" { target *-*-* } .-2 }
