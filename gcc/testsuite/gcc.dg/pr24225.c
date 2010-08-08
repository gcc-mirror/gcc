/* This was an ICE caused by the compiler-generated stack save/restore
   statements around s[b].  */
/* { dg-do compile} */
/* { dg-options "-O1 -fprofile-arcs" } */

int
foo (int a, int b)
{
  if (a)
    return 1;
  {
    int s [b];
    return 0;
  }
}
