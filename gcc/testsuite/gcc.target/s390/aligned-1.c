/* Even symbols without explicit alignment are assumed to reside on a
   2 byte boundary, as mandated by the IBM Z ELF ABI, and therefore
   can be accessed using the larl instruction.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=z900 -fno-section-anchors" } */

extern unsigned char extern_char;
extern unsigned char extern_explicitly_aligned_char __attribute__((aligned(2)));
extern unsigned char extern_explicitly_unaligned_char __attribute__((aligned(1)));
extern unsigned char __attribute__((weak)) extern_weak_char;
extern unsigned char extern_explicitly_aligned_weak_char __attribute__((weak,aligned(2)));
extern unsigned char extern_explicitly_unaligned_weak_char __attribute__((weak,aligned(1)));

unsigned char normal_char;
unsigned char explicitly_unaligned_char __attribute__((aligned(1)));
unsigned char __attribute__((weak)) weak_char = 0;
unsigned char explicitly_aligned_weak_char __attribute__((weak,aligned(2)));
unsigned char explicitly_unaligned_weak_char __attribute__((weak,aligned(1)));

extern unsigned int extern_int;
extern unsigned int extern_explicitly_aligned_int __attribute__((aligned(4)));
extern unsigned int extern_explicitly_unaligned_int __attribute__((aligned(1)));
extern unsigned int __attribute__((weak)) extern_weak_int;
extern unsigned int extern_explicitly_aligned_weak_int __attribute__((weak,aligned(4)));
extern unsigned int extern_explicitly_unaligned_weak_int __attribute__((weak,aligned(1)));

unsigned int normal_int;
unsigned int explicitly_unaligned_int __attribute__((aligned(1)));
unsigned int __attribute__((weak)) weak_int = 0;
unsigned int explicitly_aligned_weak_int __attribute__((weak,aligned(4)));
unsigned int explicitly_unaligned_weak_int __attribute__((weak,aligned(1)));

extern const void extern_void;
extern const void extern_explicitly_aligned_void __attribute__((aligned(2)));
extern const void extern_explicitly_unaligned_void __attribute__((aligned(1)));
extern const void __attribute__((weak)) extern_weak_void;
extern const void extern_explicitly_aligned_weak_void __attribute__((weak,aligned(2)));
extern const void extern_explicitly_unaligned_weak_void __attribute__((weak,aligned(1)));


unsigned int
foo ()
{
  return extern_char + extern_explicitly_aligned_char
    + extern_explicitly_unaligned_char
    + extern_weak_char + extern_explicitly_aligned_weak_char
    + extern_explicitly_unaligned_weak_char

    + normal_char + explicitly_unaligned_char
    + weak_char + explicitly_aligned_weak_char
    + explicitly_unaligned_weak_char

    + extern_int + extern_explicitly_aligned_int
    + extern_explicitly_unaligned_int
    + extern_weak_int + extern_explicitly_aligned_weak_int
    + extern_explicitly_unaligned_weak_int

    + normal_int + explicitly_unaligned_int
    + weak_int + explicitly_aligned_weak_int
    + explicitly_unaligned_weak_int;
}

const void *f1(void) { return &extern_void; }
const void *f2(void) { return &extern_explicitly_aligned_void; }
const void *f3(void) { return &extern_explicitly_unaligned_void; }
const void *f4(void) { return &extern_weak_void; }
const void *f5(void) { return &extern_explicitly_aligned_weak_void; }
const void *f6(void) { return &extern_explicitly_unaligned_weak_void; }


/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_char\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_aligned_char\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_unaligned_char\n" 0 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_weak_char\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_aligned_weak_char\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_unaligned_weak_char\n" 0 } } */

/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,normal_char\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,explicitly_unaligned_char\n" 0 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,weak_char\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,explicitly_aligned_weak_char\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,explicitly_unaligned_weak_char\n" 0 } } */

/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_int\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_aligned_int\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_unaligned_int\n" 0 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_weak_int\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_aligned_weak_int\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_unaligned_weak_int\n" 0 } } */

/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,normal_int\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,explicitly_unaligned_int\n" 0 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,weak_int\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,explicitly_aligned_weak_int\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,explicitly_unaligned_weak_int\n" 0 } } */

/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_void\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_aligned_void\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_unaligned_void\n" 0 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_weak_void\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_aligned_weak_void\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_unaligned_weak_void\n" 0 } } */
