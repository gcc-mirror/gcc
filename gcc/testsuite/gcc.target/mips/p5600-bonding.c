/* { dg-do compile } */
/* { dg-options "-dp isa=p5600 -mtune=p5600 -mno-micromips -mno-mips16" } */
/* { dg-skip-if "Bonding needs peephole optimization." { *-*-* } { "-O0" "-O1" } { "" } } */
typedef int VINT32 __attribute__ ((vector_size((16))));

void
memory_operation (void * __restrict src, void * __restrict dest, int num)
{
  VINT32 *vsrc = (VINT32 *) src;
  VINT32 *vdest = (VINT32 *) dest;
  int i;

  for (i = 0; i < num - 1; i += 2)
  {
    vdest[i] = vdest[i] + vsrc[i];
    vdest[i + 1] = vdest[i + 1] + vsrc[i + 1];
  }
}
/* { dg-final { scan-assembler "join2_" } }  */

