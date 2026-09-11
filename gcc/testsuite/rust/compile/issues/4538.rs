#![feature(no_core)]
#![no_core]

extern "C" {
    fn printf(s: *const i8, ...);
}

fn dump_number(num: i32) {
    unsafe {
        let a = "%i\n\0";
        let c = a as *const str as *const i8;
        printf(c, num);
    }
}

fn main() -> i32 {
    let c = 'block_c: {
        break 'block_c 555;
    };
    dump_number(c);
    0
}
