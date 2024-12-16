// { dg-output "1\r*\n1\r*\n1\r*\n" }

#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! include_bytes {
    () => {{}};
}

macro_rules! my_file {
    () => {"include.txt"};
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

fn check_bytes(bytes: &[u8; 16]) {
    let the_bytes = b"hello, include!\n";

    let mut x = true;
    let mut i = 0;

    // X is true iff bytes == the_bytes
    while i < 16 {
        x = x && (bytes[i] == the_bytes[i]);
        i += 1;
    }

    print_int(x as i32);
}

fn main() -> i32 {
    let bytes1: &'static [u8; 16] = include_bytes!("include.txt");
    check_bytes(bytes1);

    let bytes2: &'static [u8; 16] = include_bytes!(my_file!());
    check_bytes(bytes2);

    let bytes3 = include_bytes!(my_file!(),);
    check_bytes(bytes3);

    0
}
