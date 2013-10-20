// PR c++/58510
// { dg-do compile { target c++11 } }

void foo()
{
  union
  {		// { dg-error "multiple" }
    int i = 0;
    char c = 0;
  };
}
