/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

int
foo (int *haystack, int needle)
{
  int i = 0;
  while (1)
    {
      if (haystack[i] == needle)
	return i;
      i++;
     }
}

/* { dg-final { scan-tree-dump {note:\s*Alignment of access forced using peeling.} "vect" } } */
/* { dg-final { scan-tree-dump {if \(prolog_loop_niters.[0-9_]+ == 0\)\n\s*goto} "vect" } } */
/* { dg-final { scan-tree-dump {ivtmp_[0-9_]+ = PHI <ivtmp_[0-9_]+\([0-9_]+\), 0\([0-9_]+\)>} "vect" } } */
/* { dg-final { scan-tree-dump {ivtmp_[0-9_]+ = ivtmp_[0-9_]+ \+ 1;} "vect" } } */
/* { dg-final { scan-tree-dump {if \(ivtmp_[0-9_]+ >= prolog_loop_niters.[0-9_]+\)\n\s*goto} "vect" } } */
/* { dg-final { scan-tree-dump {vectorized 1 loops in function} "vect" } } */
