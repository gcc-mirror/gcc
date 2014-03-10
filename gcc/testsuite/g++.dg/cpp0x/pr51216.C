// PR c++/51216
// { dg-do compile { target c++11 } }
// { dg-options "" }

void foo()
{
  int i = ({ if (1) ; });           // { dg-error "ignored" }
  int j = ({ for (;;) ; });         // { dg-error "ignored" }
  int k = ({ while (1) ; });        // { dg-error "ignored" }
  int l = ({ do { } while (1); });  // { dg-error "ignored" }
}
