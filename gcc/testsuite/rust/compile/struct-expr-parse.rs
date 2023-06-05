struct A { // { dg-warning "struct is never constructed" }
    a: u32
}

pub fn foo(a: u32) {
    (&A { a }.a, 1);
}
