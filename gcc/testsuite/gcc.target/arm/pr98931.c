/* { dg-do assemble } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-options "-march=armv8.1-m.main -O3 --param=max-completely-peeled-insns=1300 --save-temps -mthumb" } */

extern long long a[][20][26][26][22];

void
foo ()
{
  for (short d = 0; d + 1; d++)
    for (unsigned e = 0; e < 25; e += 4)
      for (unsigned f = 0; f < 25; f += 4)
        for (int g = 0; g < 21; g += 4)
          a[4][d][e][f][g] = 0;
}

/* { dg-final { scan-assembler-not {le\slr,\s\S*} } } */
