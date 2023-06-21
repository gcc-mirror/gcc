// { dg-additional-options "-w" }
// { dg-output "12\r*" }
mod modules;

extern "C" {
    fn printf(s: *const i8, ...);
}

fn main() -> i32 {
    unsafe {
        let fmt_s = "%d\n\0";
        let fmt_p = fmt_s as *const str;
        let fmt_i8 = fmt_p as *const i8;

        printf(fmt_i8, modules::return_12());
    }

    return 0;
}
