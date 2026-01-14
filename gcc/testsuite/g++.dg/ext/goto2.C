// PR c++/123551
// { dg-do compile { target c++11 } }
// { dg-options "" }

void
foo ()
{
  [] () {
    void *a = &&b;
    goto *a;
   b:;
  };
}
