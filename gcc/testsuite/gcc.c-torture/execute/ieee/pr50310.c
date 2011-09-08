/* PR target/50310 */

extern void abort (void);
double s1[4], s2[4], s3[64];

void
foo (void)
{
  int i;
  for (i = 0; i < 4; i++)
    s3[0 * 4 + i] = __builtin_isgreater (s1[i], s2[i]) ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[1 * 4 + i] = (!__builtin_isgreater (s1[i], s2[i])) ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[2 * 4 + i] = __builtin_isgreaterequal (s1[i], s2[i]) ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[3 * 4 + i] = (!__builtin_isgreaterequal (s1[i], s2[i])) ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[4 * 4 + i] = __builtin_isless (s1[i], s2[i]) ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[5 * 4 + i] = (!__builtin_isless (s1[i], s2[i])) ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[6 * 4 + i] = __builtin_islessequal (s1[i], s2[i]) ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[7 * 4 + i] = (!__builtin_islessequal (s1[i], s2[i])) ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[8 * 4 + i] = __builtin_islessgreater (s1[i], s2[i]) ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[9 * 4 + i] = (!__builtin_islessgreater (s1[i], s2[i])) ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[10 * 4 + i] = __builtin_isunordered (s1[i], s2[i]) ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[11 * 4 + i] = (!__builtin_isunordered (s1[i], s2[i])) ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[12 * 4 + i] = s1[i] > s2[i] ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[13 * 4 + i] = s1[i] <= s2[i] ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[14 * 4 + i] = s1[i] < s2[i] ? -1.0 : 0.0;
  for (i = 0; i < 4; i++)
    s3[15 * 4 + i] = s1[i] >= s2[i] ? -1.0 : 0.0;
}

int
main ()
{
  int i;
  s1[0] = 5.0;
  s1[1] = 6.0;
  s1[2] = 5.0;
  s1[3] = __builtin_nan ("");
  s2[0] = 6.0;
  s2[1] = 5.0;
  s2[2] = 5.0;
  s2[3] = 5.0;
  asm volatile ("" : : : "memory");
  foo ();
  asm volatile ("" : : : "memory");
  for (i = 0; i < 16 * 4; i++)
    if (i >= 12 * 4 && (i & 3) == 3)
      {
	if (s3[i] != 0.0) abort ();
      }
    else
      {
        static int masks[] = { 2, 2|4, 1, 1|4, 1|2, 8, 2, 1 };
        if (s3[i]
	    != (((1 << (i & 3)) & ((i & 4) ? ~masks[i / 8] : masks[i / 8]))
		? -1.0 : 0.0))
	  abort ();
      }
  return 0;
}
