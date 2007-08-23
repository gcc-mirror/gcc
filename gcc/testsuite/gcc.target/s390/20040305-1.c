
/* The testcase failed due to corrupted alias information.
   During the crossjump analyzing step the mem alias info of the
   st instructions are merged and get copied during basic block
   reordering which leads to an insn with wrong alias info.
   The scheduler afterwards exchanges the mvc and st instructions 
   not recognizing the anti dependence.  */
/* { dg-do run } */
/* { dg-options "-O3 -mtune=z990 -fno-inline" } */

extern void exit (int);
extern void abort (void);

int f;
int g;
int h;

int* x  = &f;
int* p1 = &g;
int* p2 = &h;

int
foo(void)
{

  if (*x == 0)
    {
      x = p1;         /* mvc - memory to memory */
      p1 = (int*)0;   /* st  - register to memory */
      return 1;
    }
  if (*x == 5)
    {
      f = 1;
      g = 2;

      p2 = (int*)0;   /* st */   
      return 1;
    }
}

int
main (int argc, char** argv)
{
  foo ();

  /* If the scheduler has exchanged the mvc and st instructions,
     x is 0. The expected result is &g.  */
  if (x == &g)
    exit (0);
  else
    abort ();
}
