// { dg-additional-options "-fsyntax-only" }

fn foo() {
    move |l: u32, r: u32| l + r
}

fn foo2() {
    |l: u32, r: u32| l + r
}
