/* Check that sdata-accesses are applied regardless of size or ABI.  */
/* { dg-options -mexplicit-relocs } */
/* { dg-do compile { target mips*-*-elf* } } */

struct s { int x[4]; };
struct s my_struct __attribute__((__section__(".sdata")));

int f() { return my_struct.x[0]; }

/* { dg-final { scan-assembler {gp_?rel\(my_struct} } } */
