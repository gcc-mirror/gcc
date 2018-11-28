/* PR c/88065 - ICE in -Wsizeof-pointer-memaccess on an invalid strncpy
   PR c/87297 - ICE on strncpy with an undeclared argument
   { dg-do compile }
   { dg-options "-Wall -Wsizeof-pointer-memaccess" } */

typedef __SIZE_TYPE__ size_t;

char* strncpy (char*, const char*, size_t);

struct A { char a[4], b[6]; };

void test_invalid_dst (struct A *p)
{
  strncpy (q->a, p->b, sizeof p->b);    /* { dg-error ".q. undeclared|not declared" } */
}

void test_invalid_src (struct A *p)
{
  strncpy (p->a, q->b, sizeof p->b);    /* { dg-error ".q. undeclared|not declared" } */
}

void test_invalid_bound (struct A *p)
{
  strncpy (p->a, p->b, sizeof q->b);    /* { dg-error ".q. undeclared|not declared" } */
}

/* Verify the C++ front end doesn't ICE (the verifies that the fix
   for PR c/87297 uses error_operand_p to detect the invalid source
   argument rather than just checking its equality to error_mark_node.  */
struct B { char a[4]; };

void test_cxx_invalid_dst (struct B *p, const char *s)
{
  struct T x;	                        /* { dg-error "storage size|incomplete type|unused" } */
  __builtin_strncpy (x, s, sizeof p->a);
}
