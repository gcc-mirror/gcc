/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* Check that we can use this idiom to define out-of-line copies of built-in
   functions.  This is used by libgcc/sync.c, for example.  */
void __sync_synchronize (void)
{
  __sync_synchronize ();
}
/* { dg-final { scan-assembler "__sync_synchronize" } } */
/* { dg-final { scan-assembler "\t(lock|mfence)" } } */
/* { dg-final { scan-assembler-not "\tcall" } } */
