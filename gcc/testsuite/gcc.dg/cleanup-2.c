/* { dg-do run } */
/* { dg-options "" } */
/* Verify that cleanup works in the most basic of ways.  */

extern void exit(int);
extern void abort(void);

static void handler(void *p __attribute__((unused)))
{
  exit (0);
}

static void doit(void)
{
  int x __attribute__((cleanup (handler)));
}

int main()
{
  doit ();
  abort ();
}
