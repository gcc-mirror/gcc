struct S;

impl S {
    unsafe fn foo(self) {}
}

fn main() {
    let s = S;
    s.foo(); // { dg-error "call to unsafe method" }

    unsafe {
        s.foo();
    }
}
