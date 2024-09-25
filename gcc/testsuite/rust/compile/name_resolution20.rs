// { dg-options "-frust-name-resolution-2.0" }

pub mod foo {
    pub macro bar() {}
}

use foo::bar;

fn main() {
    bar!();
}
