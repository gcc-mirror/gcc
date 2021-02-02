// { dg-additional-options -fmodules-ts }

struct bob;
import "merge-3_a.H";

bob b  = {1};

void frob ()
{
  b.i = 7;
}
