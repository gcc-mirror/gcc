/* { dg-do compile } */

#ifdef __SIZE_TYPE__
typedef __SIZE_TYPE__ size_t;
#else
typedef unsigned int size_t;
#endif

extern int snprintf (char *, size_t, const char *, ...);
const char a[] = "";
int b;
void
get_bar ()
{
  snprintf (0, 0, "%s", &a[b]);
}
