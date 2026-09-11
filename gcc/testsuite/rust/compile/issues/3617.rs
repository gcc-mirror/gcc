#![feature(no_core)]
#![no_core]

macro_rules! quote_tokens {
    () => {
        #[macro_export]
        macro_rules! inner {
                    () => {
                        $crate::
                    }
                }
    };
}

pub fn main() {
    quote_tokens!();
}
