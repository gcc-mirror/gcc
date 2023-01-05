#[target_feature(sse)]
fn foo() {
    let a: usize = 0;
}

fn main() {
    foo() // { dg-error "requires unsafe function or block" }
}
