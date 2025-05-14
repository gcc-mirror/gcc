#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! asm {
    () => {};
}

fn main() -> i32 {
    unsafe {
        asm!(
            "add {}, 1",
            in(reg) 0
        );
    }

    // This adds two numbers num1 and num2 into num1, giving us 30
    // {0} stands for num1
    // {1} stands for num2
    let mut _num1: i32 = 10;
    let _num2: i32 = 20;
    unsafe {
        asm!(
            "add {}, {}",
            in(reg) _num2,
            out(reg) _num1,
        );
    }

    let mut _output_testing: u32 = 0;
    unsafe {
        asm!(
            "add {}, 1",
            in(reg) _num1,
            //out(reg) _,
        );
    }

    0
}
