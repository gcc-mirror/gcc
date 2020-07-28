// PR c++/96328
// { dg-do compile }
friend // { dg-error "'friend' used outside of class" }
// { dg-prune-output "expected unqualified-id at end of input" }
