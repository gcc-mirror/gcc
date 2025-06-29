// { dg-output "correct\r*" }
extern "C" {
    fn puts(s: *const i8);
}

fn main() -> i32 {
    let x = (1, 2, 3, 4);
    let mut ret = 1;

    match x {
        (1, .., 2, 4) => {
            /* should not take this path */
            unsafe { puts("wrong\0" as *const str as *const i8) }
        },
        (2, ..) => {
            /* should not take this path */
            unsafe { puts("wrong\0" as *const str as *const i8) }
        },
        (b, .., 4) => { 
            ret -= b;
            unsafe { puts("correct\0" as *const str as *const i8) }
        },
        _ => {}
    }

    ret
}