/* { dg-do compile } */

_mark (long obj, int i, char *a)
{
  (char *)&(((long *)(obj)) [i]) - a;
}
