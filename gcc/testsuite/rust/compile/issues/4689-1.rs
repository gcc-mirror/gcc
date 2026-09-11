#![feature(no_core)]
#![no_core]

pub mod collections {
    pub enum TryReserveError {
        AllocError,
        CapacityOverflow,
    }
}

pub mod test_working {
    use crate::collections::TryReserveError::{self, AllocError, CapacityOverflow};
    fn _test_function() -> TryReserveError {
        AllocError
    }
}

pub mod test_failing {
    use crate::collections::TryReserveError::{self, *};
    fn _test_function() -> TryReserveError {
        CapacityOverflow
    }
}
