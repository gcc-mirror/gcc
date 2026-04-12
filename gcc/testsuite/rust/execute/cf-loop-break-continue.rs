// { dg-options "-w" }
// { dg-output "73\r*\n37\r*\n24\r*\n24\r*\n" }
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

fn compute(limit: i32, stop_at: i32) -> i32 {
    let mut i = 0;
    let mut sum = 0;

    loop {
        if i >= limit {
            break;
        }

        i += 1;

        if i % 2 == 0 {
            continue;
        }

        if i % 3 == 0 {
            continue;
        }

        sum += i;

        if sum >= stop_at {
            break;
        }
    }

    sum
}

fn main() -> i32 {
    dump_number(compute(20, 100));
    dump_number(compute(30, 26));
    dump_number(compute(18, 17));
    dump_number(compute(15, 14));
    0
}
