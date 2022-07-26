extern "C" {
    fn printf(fmt: *const i8, ...);
}

fn main() {
    let s = "hey\0";

    printf(s as *const str as *const i8); // { dg-error "call to extern function" }
}
