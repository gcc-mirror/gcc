/* This code is from the beginning of combine_reloads in reload.c in
   GCC 3.1-20020117, with simplifications.  It compiled incorrectly
   for -O2 -fprefetch-loop-arrays for ix86 targets.  */

/* { dg-do run } */
/* { dg-options "-O2 -fprefetch-loop-arrays -w" } */
/* { dg-options "-O2 -fprefetch-loop-arrays -mtune=pentium3 -w" { target i?86-*-* } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } } */

struct reload
{
  int first_member;
  int out;
  int final_member;
};

int n_reloads;
struct reload rld[10];

static int
combine_reloads ()
{
  int i;
  int output_reload = -1;
  int secondary_out = -1;

  for (i = 0; i < n_reloads; i++)
    if (rld[i].out != 0)
      {
	if (output_reload >= 0)
	  return output_reload;
	output_reload = i;
      }
  return output_reload;
}

int
main ()
{
  n_reloads = 4;
  rld[2].out = 2;
  if (combine_reloads () != 2)
    abort ();
  exit (0);
}
