// PR middle-end/16558
// { dg-options "-Wreturn-type" }

struct C
{
  C ();
  ~C ();
};

int getref (int ndx)
{
  C d;

  if (ndx != 0) {
    C base;
    return 0;
  }
  else
    return 0;
}
