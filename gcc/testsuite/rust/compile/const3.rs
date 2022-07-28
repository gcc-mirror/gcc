fn size() -> usize {
    15
}

fn main() {
    let a = [15; size()]; // { dg-error "only functions marked as .const. are allowed to be called from constant contexts" }
}
