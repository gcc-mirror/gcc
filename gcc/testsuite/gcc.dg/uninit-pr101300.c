/* PR middle-end/101300 - -fsanitize=undefined suppresses -Wuninitialized
   for a VLA read at -O0
   { dg-do compile }
   { dg-options "-O0 -Wall -fsanitize=undefined" } */

int warn_vla_rd0 (int n)
{
  char a[n];
  return a[0];      // { dg-warning "\\\[-Wuninitialized]" }
}

int warn_vla_rd1 (int n)
{
  char a[n];
  return a[1];      // { dg-warning "\\\[-Wuninitialized]" }
}

int warn_vla_rdi (int n, int i)
{
  char a[n];
  return a[i];      // { dg-warning "\\\[-Wuninitialized]" }
}


int warn_vla_wr0_rd2_1_0 (int n)
{
  char a[n];
  a[0] = __LINE__;
  int x = a[2];     // { dg-warning "\\\[-Wuninitialized]" }
  int y = a[1];     // { dg-warning "\\\[-Wuninitialized]" }
  int z = a[0];
  return x + y + z;
}

int warn_vla_wr1_rd2_1_0 (int n)
{
  char a[n];
  a[1] = __LINE__;
  int x = a[2];     // { dg-warning "\\\[-Wuninitialized]" }
  int y = a[1];
  int z = a[0];     // { dg-warning "\\\[-Wuninitialized]" }
  return x + y + z;
}

int warn_vla_wr2_rd2_1_0 (int n)
{
  char a[n];
  a[2] = __LINE__;
  int x = a[2];
  int y = a[1];     // { dg-warning "\\\[-Wuninitialized]" }
  int z = a[0];     // { dg-warning "\\\[-Wuninitialized]" }
  return x + y + z;
}
