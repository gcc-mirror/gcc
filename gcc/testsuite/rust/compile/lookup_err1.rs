fn test() {
    fn nested() {}
}

fn main() {
    nested(); // { dg-error "Cannot find path .nested. in this scope" }
}
