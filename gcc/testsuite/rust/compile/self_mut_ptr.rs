struct MyStruct;

impl MyStruct {
    pub fn do_something(*mut self) {}
    // { dg-error "cannot pass .self. by raw pointer" "" { target *-*-* } .-1 }
    // { dg-error "failed to parse inherent impl item in inherent impl" "" { target *-*-* } .-2 }
    // { dg-error "failed to parse item in crate" "" { target *-*-* } .-3 }
}
