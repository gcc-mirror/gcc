/* PR tree-optimization/104119 - unexpected -Wformat-overflow after strlen
   in ILP32 since Ranger integration
   Verify that unlike -Wformat-overflow the sprintf optimization doesn't
   assume the length of a string isn't bounded by the size of the array
   member it's stored in.
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

typedef __SIZE_TYPE__ size_t;

void* memcpy (void*, const void*, size_t);
int snprintf (char*, size_t, const char*, ...);
char* strcpy (char*, const char*);
size_t strlen (const char*);

extern void keep_call_on_line (int);
extern void elim_call_on_line (int);

void sink (void*, ...);

struct __attribute__ ((packed)) S
{
  char a4[4], b4[4], ax[];
};

extern struct S es;

void test_extern_decl_memcpy (void)
{
  struct S *p = &es;

  /* Set strlen (P->A4) to [3, PTRDIFF - 2].   */
  memcpy (p->a4, "123", 3);
  int i = snprintf (0, 0, "%s", p->a4);
  if (i > 4)
    keep_call_on_line (__LINE__);
}

void test_extern_decl_strcpy_3 (void)
{
  struct S *p = &es;

  /* Set strlen (P->A4) to 3.   */
  strcpy (p->a4, "123");
  int i = snprintf (0, 0, "%s", p->a4);
  if (i > 4)
    elim_call_on_line (__LINE__);
}

void test_extern_decl_strcpy_X (const char *s)
{
  struct S *p = &es;

  /* Set strlen (P->A4) to [0, PTRDIFF_MAX - 2].   */
  strcpy (p->a4, s);
  int i = snprintf (0, 0, "%s", p->a4);
  if (i > 4)
    keep_call_on_line (__LINE__);
}

size_t test_extern_decl_strlen (void)
{
  struct S *p = &es;

  /* Set strlen (P->A4) to [0, PTRDIFF - 2].   */
  size_t n = strlen (p->a4);
  int i = snprintf (0, 0, "%s", p->a4);
  if (i > 4)
    keep_call_on_line (__LINE__);
  return n;
}


static struct S ss;

/* Store and read SS to prevent optimizers from assuming it's unchanged.  */

extern void set_ss (struct S *p)
{
  if (ss.a4[(unsigned char)*p->a4])
    __builtin_memcpy (&ss, p, sizeof ss);
}


void test_static_decl_memcpy (void)
{
  struct S *p = &ss;

  /* Set strlen (P->A4) to [3, PTRDIFF - 2].   */
  memcpy (p->a4, "123", 3);
  int i = snprintf (0, 0, "%s", p->a4);
  if (i > 4)
    keep_call_on_line (__LINE__);
}

void test_static_decl_strcpy_3 (void)
{
  struct S *p = &ss;

  /* Set strlen (P->A4) to 3.   */
  strcpy (p->a4, "123");
  int i = snprintf (0, 0, "%s", p->a4);
  if (i > 4)
    elim_call_on_line (__LINE__);
}

void test_static_decl_strcpy_X (const char *s)
{
  struct S *p = &ss;

  /* Set strlen (P->A4) to [0, PTRDIFF_MAX - 2].   */
  strcpy (p->a4, s);
  int i = snprintf (0, 0, "%s", p->a4);
  if (i > 4)
    keep_call_on_line (__LINE__);
}

size_t test_static_decl_strlen (void)
{
  struct S *p = &ss;

  /* Set strlen (P->A4) to [0, PTRDIFF - 2].   */
  size_t n = strlen (p->a4);
  int i = snprintf (0, 0, "%s", p->a4);
  if (i > 4)
    keep_call_on_line (__LINE__);
  return n;
}

/* { dg-final { scan-tree-dump-times "keep_call_on_line" 6 "optimized" } }
   { dg-final { scan-tree-dump-not "elim_call_on_line" "optimized" } } */
