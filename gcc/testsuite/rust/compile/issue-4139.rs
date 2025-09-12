// { dg-skip-if "" { *-*-* } { "-m32" } { "" } }
const X: i32 = const {
    let a = 0x736f6d6570736575;
    // { dg-error "integer overflows the respective type" "" { target *-*-* } .-1 }
    let b = 14;
    a + b
};
