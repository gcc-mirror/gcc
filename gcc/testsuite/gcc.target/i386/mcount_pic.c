/* PR target/63534 */
/* Check correct mcount generation.  */
/* { dg-do run } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -fpic -pg -save-temps" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler "mcount" } } */
/* { dg-final { scan-assembler "get_pc_thunk" } } */
/* { dg-final { cleanup-profile-file } } */
