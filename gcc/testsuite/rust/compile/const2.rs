// { dg-additional-options "-w" }

const fn foo() {
    const fn bar() {}

    bar();
}
