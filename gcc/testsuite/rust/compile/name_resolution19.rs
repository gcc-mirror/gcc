struct Marker;

fn foo(a: Marker, b: Marker) -> Marker {
    let a = b;

    a
}

fn bar() {
    let a = 15;

    fn inner() {
        // inner functions cannot capture dynamic environment
        let b = a; // { dg-error "cannot find value .a. in this scope" }
    }
}

fn main() {
    let m = foo(Marker, Marker);
}
