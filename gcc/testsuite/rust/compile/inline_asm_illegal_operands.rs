#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! asm {
    () => {}
}

fn main() {
    let mut _x: u64 = 4;
    unsafe {
       asm!(
            "add {x}, {1}",
            x = in(reg) _x, 
            x = in(reg) _x,  // { dg-error {duplicate argument named 'x'} }
        );

        asm!(
            "mov {x}, {x}",
            x = inout("eax") _x, //  { dg-error {explicit register arguments cannot have names} }
            x = inout(reg) _x, // It then proceeds to parse this line, resulting in only 1 error instead of duplication error as well.
        );
    }
    _x = 1;
}
