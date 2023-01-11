// { dg-output "E::One\r*\nE::Two\r*\nbreak!\r*\n" }

extern "C" {
    fn printf(s: *const i8, ...);
}

enum E {
    One,
    Two,
    Other,
}

fn foo() {
    let mut x = E::One;

    loop {
        match x {
            E::One => {
                let a = "E::One\n\0";
                let b = a as *const str;
                let c = b as *const i8;
                unsafe {
                    printf(c);
                }

                x = E::Two;
            }
            E::Two => {
                let a = "E::Two\n\0";
                let b = a as *const str;
                let c = b as *const i8;
                unsafe {
                    printf(c);
                }

                x = E::Other;
            }
            _ => {
                let a = "break!\n\0";
                let b = a as *const str;
                let c = b as *const i8;
                unsafe {
                    printf(c);
                }

                break;
            }
        }
    }
}

fn main() -> i32 {
    foo();

    0
}
