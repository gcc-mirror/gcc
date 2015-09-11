/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Check that the initial values are honored when necessary.  */

void link_error (void);

/* The call to link_error cannot be eliminated in this case.  */

void test1 (int param1, int param2, int x)
{
  if (param1)
    x = 3;

  if (param2)
    if (x != 3)
      link_error ();
}

/* The call to link_error cannot be eliminated in this case.  */

int global;
void test2 (int param1, int param2)
{
  if (param1)
    global = 3;

  if (param2)
    if (global != 3)
      link_error ();
}

/* In this case, we can eliminate the call, as unless "local" is set
   to 3, its value is undefined.  */

void test3 (int param1, int param2)
{
  int local;

  if (param1)
    local = 3;

  if (param2)
    if (local != 3)
      link_error ();
}

/* { dg-final { scan-tree-dump-times "link_error" 2 "optimized" } } */
