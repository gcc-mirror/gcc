/* To test that the compiler can fill all the paddings to zeroes for the 
   structures when the auto variable is partially initialized,  fully 
   initialized, or not initialized for -ftrivial-auto-var-init=zero.  */
/* { dg-do run } */
/* { dg-options "-ftrivial-auto-var-init=zero" } */

/* Structure with no padding. */
struct test_packed {
  unsigned long one;
  unsigned long two;
  unsigned long three;
  unsigned long four;
} p1;

/* Simple structure with padding likely to be covered by compiler. */
struct test_small_hole {
  unsigned long one;
  char two;
  /* 3 byte padding hole here. */
  int three;
  unsigned long four;
} sh1;

/* Try to trigger unhandled padding in a structure. */
struct test_aligned {
  unsigned int internal1;
  unsigned long long internal2;
} __attribute__((__aligned__(64)));

struct test_aligned a1;

struct test_big_hole {
  unsigned char one;
  unsigned char two;
  unsigned char three;
  /* 61 byte padding hole here. */
  struct test_aligned four;
} __attribute__((__aligned__(64))); 

struct test_big_hole bh1;

struct test_trailing_hole {
  char *one;
  char *two;
  char *three;
  char four;
  /* "sizeof(unsigned long) - 1" byte padding hole here. */
} th1;

__attribute__((noipa)) void
foo (struct test_packed *p, struct test_small_hole *sh, struct test_aligned *a,
     struct test_big_hole *bh, struct test_trailing_hole *th)
{
  p->one = 1; p->two = 2; p->three = 3; p->four = 4;
  sh->one = 11; sh->two = 12; sh->three = 13; sh->four = 14;
  a->internal1 = 21; a->internal2 = 22;
  bh->one = 31; bh->two = 32; bh->three = 33;
  bh->four.internal1 = 34; bh->four.internal2 = 35; 
  th->one = 0; th->two = 0; th->three = 0; th->four = 44;
}

int main ()
{
  struct test_packed p2;
  struct test_small_hole sh2;
  struct test_aligned a2;
  struct test_big_hole bh2;
  struct test_trailing_hole th2;

  struct test_packed p3 = {.one = 1};
  struct test_small_hole sh3 = {.two = 12};
  struct test_aligned a3 = {.internal1 = 21};
  struct test_big_hole bh3 = {.one = 31};
  struct test_trailing_hole th3 = {.three = 0};

  struct test_packed p4 = {.one = 1, .two = 2, .three = 3, .four = 4};
  struct test_small_hole sh4 = {.one = 11, .two = 12, .three = 13, .four = 14};
  struct test_aligned a4 = {.internal1 = 21, .internal2 = 22};
  struct test_big_hole bh4 = {.one = 31, .two = 32, .three = 33};
  struct test_trailing_hole th4 = {.one = 0, .two = 0, .three = 0, .four = 44};

  foo (&p1, &sh1, &a1, &bh1, &th1);
  foo (&p2, &sh2, &a2, &bh2, &th2);
  foo (&p3, &sh3, &a3, &bh3, &th3);
  bh4.four.internal1 = 34; bh4.four.internal2 = 35;

  __builtin_clear_padding (&p1);
  __builtin_clear_padding (&sh1);
  __builtin_clear_padding (&a1);
  __builtin_clear_padding (&bh1);
  __builtin_clear_padding (&th1);

  if (__builtin_memcmp (&p1, &p2, sizeof (p1))
      || __builtin_memcmp (&sh1, &sh2, sizeof (sh1))
      || __builtin_memcmp (&a1, &a2, sizeof (a1))
      || __builtin_memcmp (&bh1, &bh2, sizeof (bh1))
      || __builtin_memcmp (&th1, &th2, sizeof (th1)))
    __builtin_abort ();
  if (__builtin_memcmp (&p1, &p3, sizeof (p1))
      || __builtin_memcmp (&sh1, &sh3, sizeof (sh1))
      || __builtin_memcmp (&a1, &a3, sizeof (a1))
      || __builtin_memcmp (&bh1, &bh3, sizeof (bh1))
      || __builtin_memcmp (&th1, &th3, sizeof (th1)))
    __builtin_abort ();
  if (__builtin_memcmp (&p1, &p4, sizeof (p1))
      || __builtin_memcmp (&sh1, &sh4, sizeof (sh1))
      || __builtin_memcmp (&a1, &a4, sizeof (a1))
      || __builtin_memcmp (&bh1, &bh4, sizeof (bh1))
      || __builtin_memcmp (&th1, &th4, sizeof (th1)))
    __builtin_abort ();


  return 0;
}
