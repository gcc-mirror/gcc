// { dg-output "91\r*\n45\r*\n" }
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

fn play(b: i32) -> i32 {
    let mut res = 0;
    let mut i = 0;
    'calculation: while i < b {
        res += i;
        i += 1;
        if res + i >= 99 {
            break 'calculation;
        }
    }
    res
}

fn main() -> i32 {
    let a: i32 = play(111);
    let b: i32 = play(10);
    dump_number(a);
    dump_number(b);
    0
}
