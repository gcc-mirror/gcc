/* { dg-do assemble } */
/* Partitioning causes hot/cold section emission and breaks stabs
   debugging.  */
/* { dg-additional-options "-fno-reorder-blocks-and-partition" } */


/* This testcase requires entries in the debug_range section in DWARF which
   refer to a vague linkage function.  */

struct s
{
  ~s ();
};

bool f1 ();
s f2 (s);

template<int x> void
f3(const s & a)
{
  while (f1 () && f1 ())
    {
      s c = f2(a);
    }
}

int main()
{
   f3<0>(s ());
   return 0;
}
