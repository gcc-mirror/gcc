fn main() {
    let mut x = [1, 2, 3];
    let y: i32 = x[0];
    print_int(y);
}

extern "C" {
    fn printf(s: *const i8, ...);
}

fn print_int(value: i32) {
    let s = "%d\n\0";
    let s_p = s as *const str;
    let c_p = s_p as *const i8;
    unsafe {
        printf(c_p, value as isize);
    }
}
