// { dg-output "999\r*\n3000\r*\n" }
#![feature(no_core)]
#![no_core]
extern "C" {
    fn printf(s: *const i8, ...);
}
fn dump_number(num: i32) {
    unsafe {
        let fmt = "%i\n\0";
        let c = fmt as *const str as *const i8;
        printf(c, num);
    }
}

fn run() -> i32 {
    let mut outer = 0;
    let mut score = 0;

    'outer: while outer < 5 {
        let mut inner = 0;
        'inner: loop {
            if inner == 3 {
                break 'outer;
            }
            score += 1000;
            inner += 1;
        }
        score += 1000;
        outer += 1;
    }
    score
}

fn main() -> i32 {
    let total = run();
    dump_number(999);
    dump_number(total);
    0
}
