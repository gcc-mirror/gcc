fn main() {
    let x = (1, 2, 3, 4);

    match x {
        (1, .., 2, 3, 4, 5) => {}, // { dg-error "expected a tuple with 4 elements, found one with 5 elements" }
        _ => {}
    }
}