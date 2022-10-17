/* { dg-do compile } */
/* { dg-options "-O2" } */
 
extern long pscc_a_2_3;
extern int pscc_a_1_4;

void
pscc (void)
{
  pscc_a_1_4 = __sync_fetch_and_and (&pscc_a_2_3, 1);
}

