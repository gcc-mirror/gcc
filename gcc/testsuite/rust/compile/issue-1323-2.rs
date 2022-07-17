fn print_int(value: i32) {
    let s = "%d\n\0";
    let s_p = s as *const str;
    let c_p = s_p as *const i8;
    unsafe {
        printf(c_p, value as isize);
    }
}

fn main() {
    print_int(5);
}

extern "C" {
    fn printf(s: *const i8, ...);
}
