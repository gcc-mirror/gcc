#![feature(no_core)]
#![no_core]

pub mod my_module {
    pub const MY_CONST: i32 = 42;
    pub fn my_func() {}
}

pub mod test_working {
    use crate::my_module::{self, MY_CONST};

    pub fn check() {
        let _ = MY_CONST;
        my_module::my_func(); 
    }
}

pub mod test_failing {
    use crate::my_module::{self, *};

    pub fn check() {
        let _ = MY_CONST; 
        my_func();        
        
        my_module::my_func(); 
    }
}
