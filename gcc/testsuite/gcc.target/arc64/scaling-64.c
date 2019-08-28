/* { dg-do compile } */
/* { dg-require-effective-target hs5x } */
/* { dg-options "-mll64 -O0 -mno-fpmov" } */

/* Address scaling for double loads and stores must multiply
   the "offset" with the size of a single element and not
   the double of it.  For example:

   Wrong code generation:
     ldd    r2,[r1]
     std    r2,[r0]
     ldd.as r2,[r1,1]
     std.as r2,[r0,1]
     ...

   Correct code generation:
     ldd    r2,[r1]
     std    r2,[r0]
     ldd.as r2,[r1,2]
     std.as r2,[r0,2]
     ...
*/

/* Must generate:

     ldd     r2,[r1]
     std     r2,[r0]
     ldd.as  r2,[r1,2]
     std.as  r2,[r0,2]
     ldd.as  r2,[r1,4]
     std.as  r2,[r0,4]
     ldd.as  r2,[r1,6]
     std.as  r2,[r0,6]
*/
void func()
{
  char buf[32];
  __builtin_strcpy(buf, "ABCDEFGHIJKLMNOPQRSTUVWXYZ23456");
}

/* { dg-final { scan-assembler "ldd\\s+r.,\\\[r.\\\]"   } } */
/* { dg-final { scan-assembler "std\\s+r.,\\\[r.\\\]"   } } */
/* { dg-final { scan-assembler "ldd.as\\s+r.,\\\[r.,2\\\]" } } */
/* { dg-final { scan-assembler "std.as\\s+r.,\\\[r.,2\\\]" } } */
/* { dg-final { scan-assembler "ldd.as\\s+r.,\\\[r.,4\\\]" } } */
/* { dg-final { scan-assembler "std.as\\s+r.,\\\[r.,4\\\]" } } */
/* { dg-final { scan-assembler "ldd.as\\s+r.,\\\[r.,6\\\]" } } */
/* { dg-final { scan-assembler "std.as\\s+r.,\\\[r.,6\\\]" } } */
