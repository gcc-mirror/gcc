/* { dg-do run } */
/* { dg-options "-O --param max-aliased-vops=1" } */

/* Compile with -O --param max-aliased-vops=1.  This partitions all
   the initial SFTs for 'm' which was causing the operand scanner to
   miss adding the right SFTs to p->b[2].  */
extern void abort (void);

struct X {
    int a;
    struct Y {
	int b[4];
    } b;
    struct Y c;
} m;

struct X n;

foo (int i)
{
  struct Y *p = (i > 10) ? &m.b : &n.c;
  p->b[2] = 10;
  m.b.b[3] = 6;
  n.c.b[2] = 3;
  return p->b[2] + n.c.b[2] + m.b.b[3];
}

main()
{
  if (foo (3) != 12)
    abort ();
  return 0;
}
