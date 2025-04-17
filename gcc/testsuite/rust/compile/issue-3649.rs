struct T(Box<>);
// { dg-error "could not resolve type path .Box. .E0412." "" { target *-*-* } .-1 }
