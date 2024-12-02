impl NonExistant {
    // { dg-error "failed to resolve" "" { target *-*-* } .-1 }
    fn test() {}
}

impl NotFound for NonExistant {
    // { dg-error "failed to resolve" "" { target *-*-* } .-1 }
    fn test() {}
}

trait A {}

impl A for NotFound {}
// { dg-error "failed to resolve" "" { target *-*-* } .-1 }
