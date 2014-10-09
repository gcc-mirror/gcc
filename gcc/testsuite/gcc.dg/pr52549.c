/* { dg-do compile } */

#ifdef __SIZE_TYPE__
_mark (__SIZE_TYPE__ obj, int i, char *a)
{
  (char *)&(((long *)(obj)) [i]) - a;
}
#elif __SIZEOF_POINTER__ == __SIZEOF_LONG__
_mark (long obj, int i, char *a)
{
  (char *)&(((long *)(obj)) [i]) - a;
}
#elif __SIZEOF_POINTER__ == __SIZEOF_INT__
_mark (int obj, int i, char *a)
{
  (char *)&(((int *)(obj)) [i]) - a;
}
#elif __SIZEOF_POINTER__ == __SIZEOF_LONG_LONG__
__extension__ _mark (long long obj, int i, char *a)
{
  (char *)&(((int *)(obj)) [i]) - a;
}
#endif
