static x = 3; // { dg-error "expecting ':' but '=' found" }

fn main() {
    let y = x +1;
}
