pub trait FirstTrait: SecondTrait {}
// { dg-error "cycle detected when computing the super predicates of .FirstTrait." "" { target *-*-* } .-1 }
pub trait SecondTrait: FirstTrait {}
// { dg-error "cycle detected when computing the super predicates of .SecondTrait." "" { target *-*-* } .-1 }
