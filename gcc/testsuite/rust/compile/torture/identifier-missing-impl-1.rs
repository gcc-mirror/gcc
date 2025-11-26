struct I();

impl I {
    fn () {
        // { dg-error {expecting 'identifier' but '\(' found} "" { target *-*-* } .-1 }
        // { dg-error {failed to parse inherent impl item in inherent impl} "" { target *-*-* } .-2 }
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
