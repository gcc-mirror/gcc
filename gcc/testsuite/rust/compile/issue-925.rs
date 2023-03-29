struct S;
trait A {
    fn foo(&self);
}

trait B: A {
    fn foo(&self);
}

impl A for S {
    fn foo(&self) {}
}

impl B for S {
    fn foo(&self) {}
}

fn test() {
    let a = S;
    a.foo();
    // { dg-error "multiple candidates found for method .foo." "" { target *-*-* } .-1 }
    // { dg-error "failed to type resolve expression" "" { target *-*-* } .-2 }
}
