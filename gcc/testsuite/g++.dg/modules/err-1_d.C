
import Foo;
// { dg-message "Frob\\(int, int, long int\\)" "" { target *-*-* } .-1 }

import Bar;
// { dg-message "Frob\\(long int, int, int\\)" "" { target *-*-* } .-1 }

int Three ()
{

  Frob (0L, 0, 0);

  Frob (0, 0, 0); // { dg-error "ambiguous" }
}
