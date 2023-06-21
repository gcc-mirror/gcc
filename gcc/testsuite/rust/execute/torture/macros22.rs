// { dg-output "1\r*\n2\r*\nNaN\r*\n3\r*\n" }

macro_rules! print_num {
    ($l:literal) => {{
        unsafe {
            printf("%d\n\0" as *const str as *const i8, $l);
        }
    }};
}

extern "C" {
    fn printf(s: *const i8, ...);
}

// Check to make sure that expanding macros does not break the flow of calls
fn main() -> i32 {
    print_num!(1);
    print_num!(2);

    unsafe {
        printf("NaN\n\0" as *const str as *const i8);
    }

    print_num!(3);

    0
}
