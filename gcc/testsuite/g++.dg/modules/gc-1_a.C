// { dg-additional-options "-fmodules-ts --param ggc-min-expand=0 --param ggc-min-heapsize=0" }

export module bob;
// { dg-module-cmi bob }

export int bob ();

void frink () 
{
}
