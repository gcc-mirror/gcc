// { dg-additional-options "-fmodules-ts --param ggc-min-expand=0 --param ggc-min-heapsize=0" }

import bob;
import stuart;
import kevin;

void frob () 
{
}

void stuart (int);

void quux ()
{
  bob ();
}

void toto ()
{
  stuart (1);
}

void fido ()
{
  stuart ();
  kevin ();
}

