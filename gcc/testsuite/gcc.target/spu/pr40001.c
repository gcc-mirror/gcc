/* { dg-do compile } */
/* { dg-options "-O" } */

void *
sbrk (unsigned int increment)
{
  volatile register
      __attribute__ ((__spu_vector__)) unsigned int sp_r1 __asm__ ("1");
  unsigned int sps;

  sps = __builtin_spu_extract (sp_r1, 0);
  if (sps - 4096 >= increment)
    return 0;
  else
    return ((void *) -1);
}

