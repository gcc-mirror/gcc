fn foo(_a: &i32) {}
fn bar(_a: i32) {}

mod inner {
    pub static mut a: i32 = 15;
}

fn main() {
    foo(&inner::a); // { dg-error "use of mutable static" }
    bar(inner::a); // { dg-error "use of mutable static" }

    unsafe {
        foo(&inner::a);
        bar(inner::a);
    }
}
