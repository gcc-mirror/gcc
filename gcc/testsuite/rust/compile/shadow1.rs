fn main() {
    let mut x = 5;
    let mut x;
    x = true;
    x = x + 2; // { dg-error "cannot apply operator .+. to types bool and <integer>"  }
}
