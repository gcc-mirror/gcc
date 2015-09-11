/* { dg-options "-fdump-tree-asan" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O3" } } */

int ext;

int
Perl_do_sv_dump()
{
    int freq[10];
    int i;
    int max = 0;

    if (max < ext)
      max = ext;
    
    for (i = 0; i <= max; i++)
      if (freq[i])
	ext = 0;

    if (i > 20)
      return freq[i];
    else
      return 0;    
}

/* { dg-final { scan-tree-dump-times "ASAN_CHECK" 2 "asan1" } } */
