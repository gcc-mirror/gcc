/* { dg-do compile } */

#if __SIZEOF_POINTER__ == __SIZEOF_LONG__
_mark (long obj, int i, char *a)
{
  (char *)&(((long *)(obj)) [i]) - a;
}
#elif __SIZEOF_POINTER__ == __SIZEOF_INT__
_mark (int obj, int i, char *a)
{
  (char *)&(((int *)(obj)) [i]) - a;
}
#endif
