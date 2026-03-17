#![feature(no_core)]
#![no_core]

macro_rules! ty_allowed {
    ($t:ty $b:block) => {{}};
}
