// PR c++/51216
// { dg-do compile { target c++11 } }
// { dg-options "" }

void foo()
{
  int i = ({ if (1) ; });           // { dg-error "cannot convert" }
  int j = ({ for (;;) ; });         // { dg-error "cannot convert" }
  int k = ({ while (1) ; });        // { dg-error "cannot convert" }
  int l = ({ do { } while (1); });  // { dg-error "cannot convert" }
}
