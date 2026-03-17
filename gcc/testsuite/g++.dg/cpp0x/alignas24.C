// PR c++/124307
// { dg-do compile { target c++11 } }
// { dg-options "" }

class : alignas // { dg-error "" }
