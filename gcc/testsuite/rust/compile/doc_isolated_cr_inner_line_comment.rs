#![feature(no_core)]
#![no_core]

pub fn main ()
{
// { dg-error "Isolated CR" "" { target *-*-* } .+1 }
  //! doc cr comment
}
