// { dg-output "10\r*\n15\r*\n35\r*\n" }
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

fn run() {
    let mut sum = 0;
    let mut i = 0;

    'outer: while i < 5 {
        i += 1;

        let mut j = 0;
        'inner: loop {
            j += 1;

            if j == 2 {
                continue 'outer;
            }

            if j > 3 {
                break 'inner;
            }

            sum += i * j;
        }
    }

    dump_number(10);
    dump_number(sum);
    dump_number(35);
}

fn main() -> i32 {
    run();
    0
}
