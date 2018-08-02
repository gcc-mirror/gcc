// PR c++/86184
// { dg-do run }
// { dg-options "" }

int j;
struct X {
  X() { j++; }
  operator bool() { return true; }
};

/* Only create X once.  */
bool b = X() ?: false;
bool b2 = X() ? X() : false;

int
main ()
{
  if (j != 3)
    __builtin_abort ();
}
