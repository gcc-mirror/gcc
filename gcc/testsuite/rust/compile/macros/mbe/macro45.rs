macro_rules! empty {
    () => {}; // { dg-error "found unexpected token '\}' in null denotation" }
}

fn main() {
    let a = empty!();
}
