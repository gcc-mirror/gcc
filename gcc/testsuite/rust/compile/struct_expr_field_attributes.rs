pub struct Test {
    #[cfg(not(any(
        target_os = "solaris",
        target_os = "illumos",
        target_os = "fuchsia",
        target_os = "redox",
    )))]
    value: bool,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

pub fn test() -> Test {
    Test {
        #[cfg(not(any(
            target_os = "solaris",
            target_os = "illumos",
            target_os = "fuchsia",
            target_os = "redox",
        )))]
        value: false,
    }
}
