#![feature(optin_builtin_traits)]
trait Cold {}

auto trait IsCool: Cold {}
// { dg-error "auto traits cannot have super traits .E0568." "" { target *-*-* } .-1 }
