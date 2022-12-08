/* PR debug/106719 */
/* { dg-do compile { target sync_char_short } } */
/* { dg-options "-O2 -fcompare-debug" } */

extern short int esi, easi[2];

void
foo (void)
{
  short int *psi = &easi[1];
  __atomic_nand_fetch (psi, esi, 0);
  psi = &easi[1];
}
