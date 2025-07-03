mod a {
    pub mod b {
        pub fn baz() {}
    }
    pub fn baz() {}
}

use a::b::*;
use a::*;

pub fn func() {
    baz(); // { dg-error ".baz. is ambiguous .E0659." }
}
