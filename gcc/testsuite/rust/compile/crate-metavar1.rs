macro_rules! foo {
    () => {
        $crate::inner::bar()
    }
}

pub mod inner {
    pub fn bar() { }
}

fn main() {
    foo!();
    crate::inner::bar();
}
