/* Verify that we don't ICE by forming invalid addresses for unaligned
   doubleword loads (originally for PPC64).  */

struct a
{
 unsigned int x;
 unsigned short y;
} __attribute__((packed));

struct b {
 struct a rep;
 unsigned long long seq;
} __attribute__((packed));

struct c {
 int x;
 struct a a[5460];
 struct b b;
};

extern void use_ull(unsigned long long);
extern void f(struct c *i) {
  use_ull(i->b.seq);
  return;
}
