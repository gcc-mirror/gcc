extern "C" {
    fn printf(fmt: *const i8, _: ...);
}

fn main() -> i32 {
    unsafe {
        printf(
            "%s" as *const str as *const i8,
            "Message" as *const str as *const i8,
        );
    }

    0
}
