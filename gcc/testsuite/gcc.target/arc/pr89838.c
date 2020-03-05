/* { dg-do compile } */
/* { dg-require-effective-target tls } */
/* { dg-options "-O2" } */

extern void foo (void);
extern void bar (void *);

struct {
  int __attribute__(()) a;
  int __attribute__(()) b;
} __thread c __attribute__((tls_model("initial-exec")));

void foo (void)
{
  bar (&c.b);
}
