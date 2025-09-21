const N: usize = 2;
const ARR: [i32; N] = [42; X];
// { dg-error {cannot find value .X. in this scope \[E0425\]} "" { target *-*-* } .-1 }
// { dg-error {mismatched types, expected .\[i32; 2]. but got .<tyty::error>. \[E0308\]} "" { target *-*-* } .-2 }
// { dg-error {mismatched types, expected .usize. but got .bool. \[E0308\]} "" { target *-*-* } .-3 }
const X: bool = (N[0] == 99) && (ARR[0] == 0);
// { dg-error {the type .usize. cannot be indexed by .<integer>. \[E0277\]} "" { target *-*-* } .-1 }
// { dg-error {mismatched types, expected .<tyty::error>. but got .<integer>. \[E0308\]} "" { target *-*-* } .-2 }

fn main() {
    let _ = X;
}
