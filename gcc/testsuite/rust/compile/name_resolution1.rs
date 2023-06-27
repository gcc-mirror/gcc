fn outer() {
    inner();

    fn inner() {}
}

fn main() {
    outer();
}
