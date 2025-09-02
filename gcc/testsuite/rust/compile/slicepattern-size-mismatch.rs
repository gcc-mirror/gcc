fn main() {
    let arr = [0, 1];

    match arr {
        [0, 1, 2] => {} // { dg-error "pattern requires 3 elements but array has 2 .E0527." }
        _ => {}
    }
}