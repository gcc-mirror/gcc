// { dg-output "a\r*\nseven\r*\nquote\r*\nelse" }

extern "C" {
    fn printf(s: *const i8, ...);
}

fn foo(x: u8) {
    match x {
        b'a' => {
            let a = "a\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }

        b'\x07' => {
            let a = "seven\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }

        b'\'' => {
            let a = "quote\n\0";
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
    let x: u8 = 7;

    foo(b'a');
    foo(x);
    foo(b'\'');
    foo(b'\\');

    0
}
