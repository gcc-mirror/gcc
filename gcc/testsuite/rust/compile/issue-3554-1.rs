trait Tr {
    fn foo();

    fn bar(&self) {
        self.foo()
        // { dg-error "no method named .foo. found in the current scope .E0599." "" { target *-*-* } .-1 }
    }
}
