struct Foo {
    a: f32,
    b: f32,
}

fn main() {
    let a = Foo { 0: 10.0, 1: 20.0 }; // { dg-error "failed to resolve type for field" }
    // { dg-error "unknown field" "" { target *-*-* } .-1 }
    // { dg-prune-output "compilation terminated" }
}
