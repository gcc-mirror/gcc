fn bar() {}

const fn foo() {
    bar(); // { dg-error "only functions marked as .const. are allowed to be called from constant contexts" }
}

