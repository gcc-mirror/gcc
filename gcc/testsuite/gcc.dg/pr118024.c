/* PR middle-end/118024 */
/* { dg-do compile } */
/* { dg-options "-fstrub=all" } */
/* { dg-require-effective-target strub } */

void *realloc (void *, __SIZE_TYPE__);
void *reallocarray (void *);
void *reallocarray (void *) __attribute__((__malloc__(reallocarray)));

void *
foo (void)
{
  char *buf = reallocarray (0);
  return realloc (buf, 1);
}
