/* { dg-do assemble } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-options "-O2 -marm -mfloat-abi=soft" } */

/* Check that we don't try to emit STRD in ARM state with
   odd starting register.  */

struct S {
  double M0;
} __attribute((aligned)) __attribute((packed));

void bar(void *);

void foo(int x, struct S y) {
  asm("" : : : "r1", "r8", "r7", "r4");
  y.M0 ?: bar(0);
  bar(__builtin_alloca(8));
}
