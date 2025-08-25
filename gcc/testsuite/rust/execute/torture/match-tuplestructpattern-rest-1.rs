// { dg-output "correct\r*" }
extern "C" {
    fn puts(s: *const i8);
}

fn main() -> i32 {
    struct A (i32, i32, i32);
    let a = A (0, 1, 2);
    let mut ret = 1;

    match a {
        A (1, ..) => {
            /* should not take this path */
            unsafe { puts("wrong\0" as *const str as *const i8) }
        }
        A (0, b, ..) => { 
            ret -= b;
            unsafe { puts("correct\0" as *const str as *const i8) }
        },
        _ => {}
    }

    ret
}
