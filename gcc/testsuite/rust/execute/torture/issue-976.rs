/* { dg-output "hi\r*" } */
fn main() -> i32 {
    {
        extern "C" {
            fn puts(s: *const i8);
        }

        unsafe {
            puts("hi\0" as *const str as *const i8);
        }
    }

    0
}
