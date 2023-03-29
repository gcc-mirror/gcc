extern "C" {
    static VALUE: char;
}

fn main() {
    let _ = VALUE; // { dg-error "use of extern static" }
    unsafe {
        let _ = VALUE;
    }
}
