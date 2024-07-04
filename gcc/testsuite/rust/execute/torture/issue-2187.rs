/* { dg-output "L1\n\L2\nL3\nL4" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

fn main() -> i32 {
    let A = b"L1
L2\0";
    let B = "L3
L4\0";

    unsafe {
        let a = "%s\n\0";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, A);
        printf(c, B);
    }

    0
}

