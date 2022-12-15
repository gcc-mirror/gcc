/* { dg-do compile } */
/* { dg-options "-O -Wdangling-pointer" } */

void alloc(void **p);
void false_dangling(char **p)
{
  {
    void *q;
    alloc(&q);
    *p = q;
  }
  char *a = __builtin_memcpy(*p, "", 1);
  *a = 0; /* { dg-bogus "dangling" } */
}
