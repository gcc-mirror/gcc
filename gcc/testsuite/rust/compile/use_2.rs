mod foo {
    pub struct S;
}

use foo::S as T;

const V: T = T; // { dg-warning "unused name" }
