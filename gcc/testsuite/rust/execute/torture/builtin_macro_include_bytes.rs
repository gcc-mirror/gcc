// { dg-output "104\r*\n33\r*\n1\r*\n" }
#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! include_bytes {
    () => {{}};
}

extern "C" {
    fn printf(s: *const i8, ...);
}

fn print_int(value: i32) {
    let s = "%d\n\0" as *const str as *const i8;
    unsafe {
        printf(s, value);
    }
}

fn main() -> i32 {
    let bytes = include_bytes!("include.txt");

    print_int(bytes[0] as i32);
    print_int(bytes[14] as i32);

    let the_bytes = b"hello, include!\n";

    let x = bytes[0] == the_bytes[0]
        && bytes[1] == the_bytes[1]
        && bytes[2] == the_bytes[2]
        && bytes[3] == the_bytes[3]
        && bytes[4] == the_bytes[4]
        && bytes[5] == the_bytes[5]
        && bytes[6] == the_bytes[6]
        && bytes[7] == the_bytes[7]
        && bytes[8] == the_bytes[8]
        && bytes[9] == the_bytes[9]
        && bytes[10] == the_bytes[10]
        && bytes[11] == the_bytes[11]
        && bytes[12] == the_bytes[12]
        && bytes[13] == the_bytes[13]
        && bytes[14] == the_bytes[14]
        && bytes[15] == the_bytes[15];

    print_int(x as i32);

    0
}
