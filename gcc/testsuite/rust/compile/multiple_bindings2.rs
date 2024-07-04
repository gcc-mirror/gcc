fn main()
{
  match (1, 2) {
    (a, a) => {},
  }
  // { dg-error "identifier .a. is bound more than once in the same pattern .E0416." "" { target *-*-* } .-2 }

  if let (a, a) = (1, 2) {}
  // { dg-error "identifier .a. is bound more than once in the same pattern .E0416." "" { target *-*-* } .-1 }

  let (a, a) = (1, 2);
  // { dg-error "identifier .a. is bound more than once in the same pattern .E0416." "" { target *-*-* } .-1 }

}
