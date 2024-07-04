pub const BAR: u32 = { // { dg-warning "unused name" }
    let ret = outer();

    const fn outer() -> u32 {
        0
    }

    ret
};
