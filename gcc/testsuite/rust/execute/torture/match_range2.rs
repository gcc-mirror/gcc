// { dg-output "lowercase\r*\nuppercase\r*\nother\r*\n" }

extern "C" {
    fn printf(s: *const i8, ...);
}

const BIG_A: char = 'A';
const BIG_Z: char = 'Z';

fn bar(x: char) {
    match x {
        'a'..='z' => {
            let a = "lowercase\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }
        BIG_A..=BIG_Z => {
            let a = "uppercase\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }
        _ => {
            let a = "other\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            unsafe {
                printf(c);
            }
        }
    }
}

fn main() -> i32 {
    bar('b');
    bar('X');
    bar('!');

    0
}
