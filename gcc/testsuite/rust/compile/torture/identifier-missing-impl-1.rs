struct I();

impl I {
    fn () {
        // { dg-error {expecting 'identifier' but '\(' found} "" { target *-*-* } .-1 }
        // { dg-error {failed to parse inherent impl item in inherent impl} "" { target *-*-* } .-2 }
        // { dg-error {failed to parse item in crate} "" { target *-*-* } .-3 }
    }
}

impl I {
    unsafe fn () {
        // { dg-error {expecting 'identifier' but '\(' found} "" { xfail *-*-* } .-1 }
    }
}

fn main() {
    let _i = I();
}
