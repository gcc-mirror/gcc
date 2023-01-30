fn main() {
    let _ = 42;
    let a = _ + 123; // { dg-error "use of '_' is not allowed on the right-side of an assignment" }
                     // { dg-error {failed to parse expression in let statement} "" { target *-*-* } .-1  }
                     // { dg-error {failed to parse statement or expression without block in block expression} "" { target *-*-* } .-2 }
                     // { dg-error {unrecognised token '\}' for start of item} "" { target *-*-* } .+2 }
                     // { dg-error {failed to parse item in crate} "" { target *-*-* } .+1 }
}
