/* { dg-options "-O3 -mcpu=v6.00.a -mxl-barrel-shift -mno-xl-soft-mul -mxl-pattern-compare -mxl-multiply-high" } */

volatile int m1, m2, m3;
volatile unsigned int u1, u2, u3;
volatile long l1, l2;
volatile long long llp;

volatile unsigned long ul1, ul2;
volatile unsigned long long ullp;

int test_mul () {

  /* { dg-final { scan-assembler "mul\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
    m1 = m2 * m3 ;

  /* { dg-final { scan-assembler "muli\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),(0x\[0-9a-fA-F]+|\[+-]*\[0-9]+)" } } */
    m3 = m1 * 1234 ;    

  /* { dg-final { scan-assembler "mulh\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
    llp = (long long)l1 * l2;

  /* { dg-final { scan-assembler "mulhu\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
    ullp = (unsigned long long)ul1 * ul2;

  /* { dg-final { scan-assembler "mulhsu\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
    llp = (long long)l1 * ul2;        

  /* { dg-final { scan-assembler "bslli\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),25" } } */
    m3 = m2 << 25;

  /* { dg-final { scan-assembler "bsll\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
    m2 = m1 << m3;

  /* { dg-final { scan-assembler "bsrai\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),25" } } */
    m3 = m2 >> 25;

  /* { dg-final { scan-assembler "bsra\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
    m2 = m1 >> m3;

  /* { dg-final { scan-assembler-not "idiv" } } */
    m1 = m2 / m1;

  /* { dg-final { scan-assembler-not "idivu" } } */
    u1 = u2 / u3;    

  /* { dg-final { scan-assembler "pcmpne\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
    m3 = (m3 != m1);

  /* { dg-final { scan-assembler "pcmpeq\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
    return (m1 == m2);

}

