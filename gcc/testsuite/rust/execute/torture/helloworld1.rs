/* { dg-output "Hello World\r*" }*/
extern "C" {
    fn puts(s: *const i8);
}

fn main() -> i32 {
    unsafe {
        let a = "Hello World";
        let b = a as *const str;
        let c = b as *const i8;

        puts(c);
    }
    0
}
