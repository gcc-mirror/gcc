// { dg-output "182 is more than 100\r*\n55 is less than 100\r*\n" }

extern "C" {
    fn printf(s: *const i8, ...);
}

fn foo(x: bool) -> i32 {
    match x {
        true => {
            return 182;
        }
        false => {
            return 55;
        }
    }
}

fn bar(y: i32) {
    match y < 100 {
        true => {
            let a = "%i is less than 100\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            unsafe {
                printf(c, y);
            }
        }
        _ => {
            let a = "%i is more than 100\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            unsafe {
                printf(c, y);
            }
        }
    }
}

fn main() -> i32 {
    let a = foo(true);
    let b = foo(false);

    bar(a);
    bar(b);

    0
}
