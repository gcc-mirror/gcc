// { dg-output "2\r*" }
extern "C" {
    fn printf(s: *const i8, ...);
}

fn print_int(value: i32) {
    let s = "%d\n\0";
    let s_p = s as *const str;
    let c_p = s_p as *const i8;
    unsafe {
        printf(c_p, value);
    }
}

macro_rules! add_exprs {
    ($($e:expr)?) => (0 $(+ $e)?)
}

fn main() -> i32 {
    // 2
    let a = add_exprs!(2);
    print_int(a);

    0
}
