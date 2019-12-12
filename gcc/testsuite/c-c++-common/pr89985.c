/* Ensure that -Waddress-of-packed-member doesn't emit notes when
   suppressed via -w, rather than -Wno-address-of-packed-member.  */

/* { dg-do compile } */
/* { dg-options "-w" } */

struct a { /* { dg-bogus "defined here" } */
  void *ptr;
} __attribute__((packed));

struct b { /* { dg-bogus "defined here" } */
  void *ptr;
};

void
test (struct a *p)
{
  struct b *q = (struct b *)p;
}
