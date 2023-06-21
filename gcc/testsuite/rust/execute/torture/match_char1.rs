// { dg-output "amazing\r*\nwildcard\r*\ncompiler\r*\nproductivity\r*\n" }

extern "C" {
    fn printf(s: *const i8, ...);
}

fn foo(x: char) {
    match x {
        'a' => {
            let a = "amazing\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }

        'c' => {
            let a = "compiler\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }

        'p' => {
            let a = "productivity\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }

        _ => {
            let a = "wildcard\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }
    }
}

fn main() -> i32 {
    let p = 'p';

    foo('a');
    foo('b');
    foo('c');
    foo(p);

    0
}
