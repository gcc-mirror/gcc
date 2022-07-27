fn foo() -> i32 {
    let a = 15;
    let p_a = &a as *const i32;

    unsafe { *p_a }
}

unsafe fn bar() -> i32 {
    let a = 15;
    let p_a = &a as *const i32;

    *p_a
}

fn baz() -> i32 {
    let a = 15;
    let p_a = &a as *const i32;

    *p_a // { dg-error "dereference of raw pointer" }
}

unsafe fn qux() -> i32 {
    let a = 15;
    let p_a = &a as *const i32;

    unsafe {}

    *p_a
}
