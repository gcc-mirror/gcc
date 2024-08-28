#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! asm {
    () => {};
}

fn main() {
    let mut _num1: i32 = 10;
    let mut _num2: i32 = 10;
    unsafe {
        asm!(
            "mov {}, 4",
            out(reg) _num1,
            out(reg) _num2,
        );
    }
}
