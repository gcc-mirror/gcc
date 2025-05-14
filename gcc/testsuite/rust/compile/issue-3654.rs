type Meeshka = Mow<!>;
// { dg-error "generic arguments are not allowed for this type .E0109." "" { target *-*-* } .-1 }
type Mow = &'static fn(!) -> !;
