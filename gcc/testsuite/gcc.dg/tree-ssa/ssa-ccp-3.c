/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-ccp" } */

extern void link_error (void);

/* some addresses clearly cannot be equal, check that some address
    expressions can be evaluated as constants */

char g1, g2;
void test6 (char p1, char p2)
{
  char l1 = 1, l2 = 2;
  static char s1 = 5, s2 = 7;
  if (&l1 == &l2)
    link_error ();

  if (&p1 == &p2)
    link_error ();

  if (&s1 == &s2)
    link_error ();

  if (&g1 == &g2)
    link_error ();
  
  if (&p1 == &l1)
    link_error (); 

  if (&p1 == &s1)
    link_error (); 

  if (&p1 == &l2)
    link_error (); 

  if (&p1 == &g1)
    link_error (); 

  if (&l1 == &g1)
    link_error (); 

  if (&s1 == &g1)
    link_error (); 
}

extern void *alloc (int) __attribute__ ((malloc));
char gca1[128];
char* __restrict__ rgc1;
char* test66 (char * __restrict__ rp1, char * __restrict__ rp2, char *p1)
{
  char * __restrict__ rl1 = p1;
  char * l1 = (char*) alloc (20);

  if (l1 == rgc1)
    link_error ();

  if (l1 == rp1)
    link_error ();

  if (l1 == rl1)
    link_error ();

  if (l1 == gca1)
    link_error ();

  if (rl1 == rgc1)
    link_error ();

  if (rl1 == rp1)
    link_error ();

  if (rl1 == gca1)
    link_error ();
  
  if (rp1 == rp2)
    link_error ();

  if (rp1 == rgc1)
    link_error ();
  
  if (rp1 == gca1)
    link_error ();

  if (gca1 == rgc1)
    link_error ();

}

int gci1[128];
int* __restrict__ rgi1;
int* test666 (int * __restrict__ rp1, int * __restrict__ rp2, int *p1)
{
  int * __restrict__ rl1 = p1;
  int * l1 = (int*) alloc (20);

  if (l1 == rgi1)
    link_error ();

  if (l1 == rp1)
    link_error ();

  if (l1 == rl1)
    link_error ();

  if (l1 == gci1)
    link_error ();

  if (rl1 == rgi1)
    link_error ();

  if (rl1 == rp1)
    link_error ();

  if (rl1 == gci1)
    link_error ();
  
  if (rp1 == rp2)
    link_error ();

  if (rp1 == rgi1)
    link_error ();
  
  if (rp1 == gci1)
    link_error ();

  if (gci1 == rgi1)
    link_error ();
}


/* There should be not link_error calls, if there is any the
   optimization has failed */
/* ??? While we indeed don't handle some of these, a couple of the
   restrict tests are incorrect.  */
/* { dg-final { scan-tree-dump-times "link_error" 0 "ccp" { xfail *-*-* } } } */
