// { dg-options "-fmodules++" }
// { dg-module-bmi "thing" }

export module thing;
module 
{
  int bar ();
}

export int baz ();
