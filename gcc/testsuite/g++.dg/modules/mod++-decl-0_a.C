// { dg-options "-fmodules++" }
// { dg-module-if "thing" }

export module thing;
module 
{
  int bar ();
}

export int baz ();
