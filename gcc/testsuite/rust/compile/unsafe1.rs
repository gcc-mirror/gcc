fn foo(_a: &i32) {}
fn bar(_a: i32) {}

static mut a: i32 = 15;

fn main() {
    foo(&a); // { dg-error "use of mutable static" }
    bar(a); // { dg-error "use of mutable static" }

    unsafe {
        foo(&a);
        bar(a);
    }
}
