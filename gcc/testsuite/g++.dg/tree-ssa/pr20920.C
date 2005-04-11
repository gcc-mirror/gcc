/* { dg-do compile } */
/* { dg-options "-O2" } */

/* This was causing a failure in the out of SSA pass because VRP was
   trying to insert assertions for SSA names that flow through
   abnormal edges.  */
void f(int) __attribute__((__noreturn__));
int d(const char *);
char * j ();

char *
foo (int x)
{
  char *path = __null;
  try
    {
      path = j ();
      if (path != __null)
        if (d (path) != 0)
          f (127);
      f (127);
    }
  catch (...) { }

  return path;
}
