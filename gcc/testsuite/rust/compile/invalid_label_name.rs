pub fn function() {
    'continue: loop {
        // { dg-error "invalid label name .'continue." "" { target *-*-* } .-1 }
        break 'extern;
        // { dg-error "invalid label name .'extern." "" { target *-*-* } .-1 }
    }

    'break: loop {
        // { dg-error "invalid label name .'break." "" { target *-*-* } .-1 }
        break 'for;
        // { dg-error "invalid label name .'for." "" { target *-*-* } .-1 }
    }

    'crate: loop {
        // { dg-error "invalid label name .'crate." "" { target *-*-* } .-1 }
        break 'loop;
        // { dg-error "invalid label name .'loop." "" { target *-*-* } .-1 }
    }

    'a: loop {
        break 'a;
    }
}
