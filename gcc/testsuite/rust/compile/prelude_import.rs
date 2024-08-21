#![feature(prelude_import)]

mod core {
    mod prelude {
        mod v1 {
            // hehe
        }
    }
}

#[prelude_import]
use core::prelude::v1::*;
