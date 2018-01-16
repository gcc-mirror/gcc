/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-optimized -std=gnu89" } */

extern void link_error (void);


/* check that cprop for variables of different types still works even
   if function calls or assignments to different types of data are
   interposed.  */

int test7 (int *intarr)
{
  extern int foo7 (int);
  int u = 7, v1;
  foo7 (u);
  v1 = u;
  if (v1 != 7)
    link_error ();
  return v1;
}

int test77 (int *arr)
{
  int u = 7, v1;
  arr[0] = 4;
  v1 = u;
  if (v1 != 7)
    link_error ();
  return v1 + arr[0];
}

int test777 (void)
{
  extern int foo(int *);
  int u = 7, v1;
  static int sarr[10];
  sarr[0] = 4;
  v1 = u;
  if (v1 != 7)
    link_error ();
  foo (sarr);
  return v1 + sarr[0];
}

int garr[10];
int test7777 (void)
{
  int u = 7, v1;
  garr[0] = 4;
  v1 = u;
  if (v1 != 7)
    link_error ();
  return v1 + garr[0];
}

int test88 (int *arr)
{
  static int l;
  int v1;
  l = 8;
  arr[0] = 4;
  v1 = l;
  if (v1 != 8)
    link_error ();
  l = foo88 (l);
  return v1 + arr[0];
}

int test888 (void)
{
  static int l;
  extern int foo(int *);
  int v1;
  static int sarr[10];
  l = 8;
  sarr[0] = 4;
  v1 = l;
  if (v1 != 8)
    link_error ();
  foo (sarr);
  l = foo88(l);
  return v1 + sarr[0];
}

int test8888 (void)
{
  static int l;
  int v1;
  l = 8;
  garr[0] = 4;
  v1 = l;
  if (v1 != 8)
    link_error ();
  return v1 + garr[0];
}



/* global var  */
int g9;
int garr9[10];
int test9 (int *intarr)
{
  extern int foo9 (int) __attribute__ ((const));
  int h, v;
  g9 = 9;
  h = foo9 (g9);
  v = g9;
  if (v != 9)
    link_error ();
  return g9;
}

int test99 (int *intarr)
{
  extern int foo99 (int) __attribute__ ((pure));
  int h, v;
  g9 = 9;
  h = foo99 (g9);
  v = g9;
  if (v != 9)
    link_error ();
  return g9;
}

/* foo9 is const because of its declaration in test9.  */
extern int foo9 (int);

int test999 (int *arr)
{
  static int l;
  int v1;
  g9 = 9;
  l = 4;
  v1 = g9;
  if (v1 != 9)
    link_error ();
  l = foo9 (l);
  return v1 + l;
}

/* foo99 is pure because of its declaration in test99.  */
extern int foo9 (int);

int test9999 (void)
{
  int v1;
  static int sarr[10];
  g9 = 9;
  sarr[0] = 4;
  v1 = g9;
  if (v1 != 9)
    link_error ();
  foo (sarr);
  g9 = foo99 (g9);
  return v1 + sarr[0];
}


int test99999 (void)
{
  int v1;
  g9 = 9;
  garr9[0] = 4;
  v1 = g9;
  if (v1 != 9)
    link_error ();
  return v1 + garr9[0];
}


/* There should be not link_error calls, if there is any the
   optimization has failed */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
