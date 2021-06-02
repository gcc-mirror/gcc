static x = 3; // { dg-error "expecting ':' but '=' found" }

fn main() {// { dg-error "failed to parse item in crate" }
    let y = x +1;
}
