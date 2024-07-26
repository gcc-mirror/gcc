#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! asm {
    () => {};
}
extern "C" {
    fn printf(s: *const i8, ...);
}

fn main() {
    let mut _x: i32 = 0;
    unsafe {
        asm!(
            "mov {}, 5",
            out(reg) _x
        );
    }
}
