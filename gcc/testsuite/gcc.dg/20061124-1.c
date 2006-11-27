/* { dg-do run } */
/* { dg-require-effective-target sync_char_short } */

/* This testcase failed on s390 because no compare instruction for
   the check of FLAG was emitted.  */

unsigned short int count = 0;
int flag = 1;

extern void abort (void);
extern void exit (int);

int
main ()
{
  __sync_add_and_fetch (&count, -1);

  if (!flag)
    abort ();
  exit (0);
}
