impl NonExistant {
    // { dg-error "could not resolve" "" { target *-*-* } .-1 }
    fn test() {}
}

impl NotFound for NonExistant {
    // { dg-error "could not resolve" "" { target *-*-* } .-1 }
    fn test() {}
}

trait A {}

impl A for NotFound {}
// { dg-error "could not resolve" "" { target *-*-* } .-1 }
