/* { dg-do compile } */

/* { dg-options "-O2 -Wno-error=incompatible-pointer-types -fdump-tree-forwprop2 -fdump-tree-optimized" } */

/*
  Two different checks are used here to ensure that this optimization
  doesn't occur before PROP_last_full_fold is set.
*/

/* { dg-final { scan-tree-dump-times "\\*s" 1 "forwprop2" } } */
/* { dg-final { scan-tree-dump-times "__builtin_strlen" 4 "forwprop2" } } */

/* { dg-final { scan-tree-dump-times "\\*s" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "void \\*\\)s" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "char.*?void \\*\\)&s" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_strlen" 1 "optimized" } } */

typedef __SIZE_TYPE__ size_t;
void a (void);
void b (void);

void modified1 (const char *s)
{
  if (__builtin_strlen (s))   // folded to if (*s)
    a ();
}

void modified2 (const char *s)
{
  /*
    folded to
    __SIZE_TYPE__ n = (*s);
  */
  __SIZE_TYPE__ n = __builtin_strlen (s);
  if (n)
    a ();
}

void unaffected1 (const char *s)
{
  /*
    this shouldn't be folded.
  */
  __SIZE_TYPE__ n = __builtin_strlen (s);
  if (n)
    a ();
  if (n > 5)
    b ();
}

void modified3 (const char *s)
{
  /*
    folded to
    __SIZE_TYPE__ n = (*s);
  */
  __SIZE_TYPE__ n = __builtin_strlen (s);
  if (n)
    a ();
  if (!n)
    b ();
}

int main (void) {
  volatile char s[] = "\0\1\1\1";

  // This ought to dereference as a char * pointer.
  __SIZE_TYPE__ n = __builtin_strlen ((int *) s); /* { dg-warning "incompatible pointer type" } */

  /*
    If the strlen call above is turned into ((int *) s)*, then it will see the
    four-byte string "\0\1\1\1" and read it as 0x01010100. This is undesirable
    given that the original strlen call would have just produced a 0.

    So, this tests to ensure that the value is 0, as we would expect.
  */
  if (n)
    return 1;
  else
    return 0;
}