/* { dg-do run } */
/* { dg-options "" } */
/* Verify that the search for function happens in the proper scope.  */

extern void exit(int);
extern void abort(void);

int main()
{
  auto void xyzzy(void *p __attribute__((unused)))
  {
    exit (0);
  }

  auto void doit ()
  {
    int x __attribute__((cleanup (xyzzy)));
  }

  doit ();
  abort ();
}
