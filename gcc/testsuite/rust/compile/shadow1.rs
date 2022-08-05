fn main() {
    let mut x = 5;
    let mut x;
    x = true;
    x = x + 2; // { dg-error "cannot apply this operator to types bool and <integer>"  }
               // { dg-error {failed to type resolve expression} "" { target *-*-* } .-1 }
}
