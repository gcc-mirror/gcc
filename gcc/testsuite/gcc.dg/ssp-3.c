/* { dg-do assemble } */
/* { dg-options "-fstack-protector-strong -O1 -frename-registers" } */
/* { dg-require-effective-target fstack_protector } */

extern int bar (const char *s, int *argc);
extern int baz (const char *s);

char
foo (const char *s)
{
  int argc;
  int ret;
  if ( !bar (s, &argc))
    ret = baz (s);
  return *s;
}
