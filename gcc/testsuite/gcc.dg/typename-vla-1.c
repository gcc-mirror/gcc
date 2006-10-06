/* PR c/21536 */
/* { dg-do run } */
/* { dg-options "-O2 -Wuninitialized" } */

extern void abort (void);
extern void exit (int);

int
main (void)
{
  int a = 1;
  if (sizeof (*(++a, (char (*)[a])0)) != 2)
    abort ();
  exit (0);
}


