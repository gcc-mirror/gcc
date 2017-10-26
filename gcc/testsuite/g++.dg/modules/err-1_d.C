
import Foo;
// { dg-message "Frob\\(int, int, long int\\)" "Foo.nms:" { target *-*-* } 0 }

import Bar;
// { dg-message "Frob\\(long int, int, int\\)" "Bar.nms:" { target *-*-* } 0 }

int Three ()
{

  Frob (0L, 0, 0);

  Frob (0, 0, 0); // { dg-error "ambiguous" }
}
