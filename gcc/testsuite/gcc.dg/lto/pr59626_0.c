/* { dg-lto-do run } */

#define ASMNAME(cname)  ASMNAME2 (__USER_LABEL_PREFIX__, cname)
#define ASMNAME2(prefix, cname) STRING (prefix) cname
#define STRING(x)    #x

int __atoi  (const char *) __asm__(ASMNAME ("atoi"));
extern inline __attribute__((always_inline,gnu_inline))
int atoi (const char *x)
{
  return __atoi (x);
}

int bar (int (*)(const char *));

int main()
{
  return bar (atoi);
}
