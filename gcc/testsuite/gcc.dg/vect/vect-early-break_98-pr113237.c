/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

long Perl_pp_split_limit;
int Perl_block_gimme();
int Perl_pp_split() {
  char strend;
  long iters;
  int gimme = Perl_block_gimme();
  while (--Perl_pp_split_limit) {
    if (gimme)
      iters++;
    if (strend)
      break;
  }
  if (iters)
    return 0;
}
