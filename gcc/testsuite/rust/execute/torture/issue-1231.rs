// { dg-additional-options "-w" }
// { dg-output "outer\r*\ninner\r*\n" }
extern "C" {
    fn printf(s: *const i8, ...);
}

fn machin() {
    unsafe {
        let a = "outer\n\0";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, 123);
    }
}

fn bidule() {
    fn machin() {
        unsafe {
            let a = "inner\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c, 123);
        }
    }

    self::machin();
    machin();
}

fn main() -> i32 {
    bidule();

    0
}
