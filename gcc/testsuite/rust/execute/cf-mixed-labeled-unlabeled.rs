// { dg-output "73\r*\n12\r*\n5\r*\n" }
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
fn mixed_flow(limit: i32) -> i32 {
    let mut sum = 0;
    let mut i = 0;

    'outer: while i < limit {
        i += 1;

        if i % 2 == 0 {
            continue;
        }

        let mut j = 0;
        loop {
            j += 1;

            if j == 2 {
                continue;
            }

            if i == 5 && j == 3 {
                continue 'outer;
            }

            if i == 9 && j == 3 {
                break 'outer;
            }

            sum += i + j;

            if j >= 4 {
                break;
            }
        }
    }

    sum
}

fn unlabeled_only(limit: i32) -> i32 {
    let mut i = 0;
    let mut acc = 0;

    while i < limit {
        i += 1;

        if i == 3 {
            continue;
        }

        acc += i;

        if i == 5 {
            break;
        }
    }

    acc
}

fn main() -> i32 {
    let a = mixed_flow(20);
    let b = unlabeled_only(9);
    dump_number(a);
    dump_number(b);
    dump_number(5);
    0
}
