// { dg-output "zero to END_RANGE\r*\nzero to END_RANGE\r*\nelse\r*\n" }

extern "C" {
    fn printf(s: *const i8, ...);
}

const END_RANGE: i32 = 15;

fn foo(x: i32) {
    match x {
        0..=END_RANGE => {
            let a = "zero to END_RANGE\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }

        _ => {
            let a = "else\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }
    }
}

fn main() -> i32 {
    foo(11);
    foo(15);
    foo(21);

    0
}
