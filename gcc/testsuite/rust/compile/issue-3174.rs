extern "C" {
    fn printf(s: *const i8, ...);
}

enum Option {
    Some(i32),
    None,
}

impl Option {
    fn add(&mut self) {
        match *self {
            Option::Some(ref mut a) => *a += 1,
            Option::None => {}
        }
    }
}

fn main() {
    unsafe {
        let mut a = Option::None;
        a.add();
        let _s = "%d\n\0";
        let _s = _s as *const str;
        let s = _s as *const i8;
        printf(s, a);
    }
}
