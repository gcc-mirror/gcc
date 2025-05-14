// { dg-additional-options "-frust-name-resolution-2.0" }
pub fn function() {
    'continue: loop {
        // { dg-error "invalid label name .'continue." "" { target *-*-* } .-1 }
        break 'extern;
        // { dg-error "invalid label name .'extern." "" { target *-*-* } .-1 }
        // { dg-error "use of undeclared label .'extern." "" { target *-*-* } .-2 }
    }

    'break: loop {
        // { dg-error "invalid label name .'break." "" { target *-*-* } .-1 }
        break 'for;
        // { dg-error "invalid label name .'for." "" { target *-*-* } .-1 }
        // { dg-error "use of undeclared label .'for." "" { target *-*-* } .-2 }
    }

    'crate: loop {
        // { dg-error "invalid label name .'crate." "" { target *-*-* } .-1 }
        break 'loop;
        // { dg-error "invalid label name .'loop." "" { target *-*-* } .-1 }
        // { dg-error "use of undeclared label .'loop." "" { target *-*-* } .-2 }
    }

    'a: loop {
        break 'a;
    }
}
