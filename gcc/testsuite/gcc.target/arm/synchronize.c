/* { dg-final { scan-assembler "__sync_synchronize|dmb|mcr" { target arm*-*-linux-* } } } */

void *foo (void)
{
  __sync_synchronize();
}
