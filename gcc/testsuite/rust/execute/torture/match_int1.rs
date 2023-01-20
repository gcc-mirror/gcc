// { dg-output "other!\r*\nother!\r*\nother!\r*\nfifteen!\r*\nfifteen!\r*\nother!\r*\nother!\r*\nfifteen!\r*\n" }

extern "C" {
    fn printf(s: *const i8, ...);
}

fn foo_i32(x: i32) {
    match x {
        15 => {
            let a = "fifteen!\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }

        _ => {
            let a = "other!\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }
    }
}

fn foo_isize(x: isize) {
    match x {
        15 => {
            let a = "fifteen!\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }

        _ => {
            let a = "other!\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }
    }
}

fn foo_u32(x: u32) {
    match x {
        15 => {
            let a = "fifteen!\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }

        _ => {
            let a = "other!\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }
    }
}

fn foo_usize(x: usize) {
    match x {
        15 => {
            let a = "fifteen!\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }

        _ => {
            let a = "other!\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }
    }
}

fn main() -> i32 {
    let x = -2;
    foo_i32(x);
    foo_i32(334);
    foo_isize(-4768);
    foo_isize(15);

    let y = 127;
    foo_u32(15);
    foo_u32(y);
    foo_usize(2394);
    foo_usize(15);

    0
}
