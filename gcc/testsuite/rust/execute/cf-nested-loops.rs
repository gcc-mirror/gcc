// { dg-output "1\r*\n2\r*\n3\r*\n1\r*\n99\r*\n" }
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

fn play1(b: i32) -> i32 {
    'low: loop {
        dump_number(1);
        'mid: loop {
            dump_number(2);
            'high: loop {
                dump_number(3);
                break 'mid;
                dump_number(9);
            }
            dump_number(7);
        }
        dump_number(1);
        break 'low;
    }
    b
}

fn main() -> i32 {
    dump_number(play1(99));
    0
}
