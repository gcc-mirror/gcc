fn test() {
    let a;

    // FIXME: Unimplemented features
    a = if true { // { dg-error "expected .T.. got .!." }
        return;
    } else {
        return;
    };
}

fn main() {
    test();
}
