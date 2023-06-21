/* { dg-output "Hello World 123\r*\n" }*/
extern "C" {
    fn printf(s: *const i8, ...);
}

fn main() -> i32 {
    unsafe {
        let a = "Hello World %i\n";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, 123);
    }
    0
}
