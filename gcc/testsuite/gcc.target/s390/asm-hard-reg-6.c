/* { dg-do compile } */
/* { dg-options "-O2" } */

void
test (void)
{
  // GPRs
  {
    int a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p;
    __asm__ __volatile__ ("%0 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13 %14"
        : "=r" (a),
          "=r" (b),
          "=r" (c),
          "=r" (d),
          "=r" (e),
          "=r" (f),
          "=r" (g),
          "=r" (h),
          "=r" (i),
          "=r" (j),
          "=r" (k),
          "=r" (l),
          "=r" (m),
          "=r" (n),
          "=r" (o));
    __asm__ __volatile__ ("%0 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13 %14"
        : "={r0}" (a),
          "={r1}" (b),
          "={r2}" (c),
          "={r3}" (d),
          "={r4}" (e),
          "={r5}" (f),
          "={r6}" (g),
          "={r7}" (h),
          "={r8}" (i),
          "={r9}" (j),
          "={r10}" (k),
          "={r11}" (l),
          "={r12}" (m),
          "={r13}" (n),
          "={r14}" (o));
    __asm__ __volatile__ ("%0 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13 %14 %15" /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
        : "=r" (a),
          "=r" (b),
          "=r" (c),
          "=r" (d),
          "=r" (e),
          "=r" (f),
          "=r" (g),
          "=r" (h),
          "=r" (i),
          "=r" (j),
          "=r" (k),
          "=r" (l),
          "=r" (m),
          "=r" (n),
          "=r" (o),
          "=r" (p));
    __asm__ __volatile__ ("%0 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13 %14 %15" /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
        : "=r" (a),
          "=r" (b),
          "=r" (c),
          "=r" (d),
          "=r" (e),
          "=r" (f),
          "=r" (g),
          "=r" (h),
          "=r" (i),
          "=r" (j),
          "=r" (k),
          "=r" (l),
          "=r" (m),
          "=r" (n),
          "=r" (o),
          "={r4}" (p));
  }

  // FPRs
  {
    float a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q;
    __asm__ __volatile__ ("%0 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13 %14 %15"
        : "=f" (a),
          "=f" (b),
          "=f" (c),
          "=f" (d),
          "=f" (e),
          "=f" (f),
          "=f" (g),
          "=f" (h),
          "=f" (i),
          "=f" (j),
          "=f" (k),
          "=f" (l),
          "=f" (m),
          "=f" (n),
          "=f" (o),
          "=f" (p));
    __asm__ __volatile__ ("%0 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13 %14 %15"
        : "={f0}" (a),
          "={f1}" (b),
          "={f2}" (c),
          "={f3}" (d),
          "={f4}" (e),
          "={f5}" (f),
          "={f6}" (g),
          "={f7}" (h),
          "={f8}" (i),
          "={f9}" (j),
          "={f10}" (k),
          "={f11}" (l),
          "={f12}" (m),
          "={f13}" (n),
          "={f14}" (o),
          "={f15}" (p));
    __asm__ __volatile__ ("%0 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13 %14 %15 %16" /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
        : "=f" (a),
          "=f" (b),
          "=f" (c),
          "=f" (d),
          "=f" (e),
          "=f" (f),
          "=f" (g),
          "=f" (h),
          "=f" (i),
          "=f" (j),
          "=f" (k),
          "=f" (l),
          "=f" (m),
          "=f" (n),
          "=f" (o),
          "=f" (p),
          "=f" (q));
    __asm__ __volatile__ ("%0 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13 %14 %15 %16" /* { dg-error "'asm' operand has impossible constraints or there are not enough registers" } */
        : "=f" (a),
          "=f" (b),
          "=f" (c),
          "=f" (d),
          "=f" (e),
          "=f" (f),
          "=f" (g),
          "=f" (h),
          "=f" (i),
          "=f" (j),
          "=f" (k),
          "=f" (l),
          "=f" (m),
          "=f" (n),
          "=f" (o),
          "=f" (p),
          "={f4}" (q));
  }
}
