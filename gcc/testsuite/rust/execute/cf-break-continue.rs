// { dg-options "-w" }
// { dg-output "prime\r*\nnot_prime\r*\nprime\r*\nprime\r*\nnot_prime\r*\n" }
#![feature(no_core)]
#![no_core]
extern "C" {
    fn puts(s: *const i8);
}
fn dump(message: &str) {
    unsafe {
        let b = message as *const str;
        let c = b as *const i8;
        puts(c);
    }
}
fn is_prime(number: i32) -> bool {
    if number <= 1 {
        return false;
    }
    let mut i = 1;
    'prime: loop {
        i += 1;
        if i * i >= number {
            break 'prime;
        }
        if number % i != 0 {
            continue 'prime;
        }
        return false;
    }
    return true;
}

fn debug_prime(number: i32) {
    let state = is_prime(number);
    if state {
        dump("prime");
    } else {
        dump("not_prime");
    }
}

fn main() -> i32 {
    debug_prime(11);
    debug_prime(12);
    debug_prime(13);
    debug_prime(17);
    debug_prime(100);
    0
}
