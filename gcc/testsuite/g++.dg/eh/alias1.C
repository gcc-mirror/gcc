// { dg-do run }
// { dg-options "-O3" }
/* PR c++/28139: disjoint alias sets for the store from
   expand_start_catch_block than for loading P result in P being loaded
   before it is initialized for sh-elf.  */

extern "C" {
void exit (int) __attribute__ ((noreturn));
}

int i_glob = 42;
int *p0 = &i_glob;
typedef int **ipp;

void
g (int i)
{
  if (!i_glob)
    exit ((__SIZE_TYPE__) & i);
}

static void
h ()
{
  throw &p0;
}

int
main()
{
  g (42);
  try
    {
     h ();
    }
  catch (const ipp &p)
    {
      if (**p != 42)
	exit (1);
    }
  return 0;
}
