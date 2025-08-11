mod intrinsic {
    pub fn foo() {}
}

pub mod a {
    pub fn b() {
        use crate::intrinsic;
        intrinsic::foo();
    }
}
