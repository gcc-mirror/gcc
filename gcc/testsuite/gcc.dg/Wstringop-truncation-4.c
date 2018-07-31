/* PR tree-optimization/85700 - Spurious -Wstringop-truncation warning
   with strncat
   { dg-do compile }
   { dg-options "-O2 -Wno-stringop-overflow -Wstringop-truncation -ftrack-macro-expansion=0" } */

#define NOIPA __attribute__ ((noipa))
#define strncat __builtin_strncat
#define strlen __builtin_strlen

extern char a4[4], b4[4], ax[];

NOIPA void cat_a4_s1_1 (void)
{
  /* There is no truncation here but since the bound of 1 equals
     the length of the source string it's likely a mistake that
     could cause overflow so it's diagnosed by -Wstringop-overflow */
  strncat (a4, "1", 1);
}

NOIPA void cat_a4_s1_2 (void)
{
  strncat (a4, "1", 2);
}

NOIPA void cat_a4_s1_3 (void)
{
  strncat (a4, "1", 3);
}

NOIPA void cat_a4_s1_4 (void)
{
  /* There is no truncation here but since the bound of 1 equals
     the length of the source string it's likely a mistake that
     could cause overflow so it's diagnosed by -Wstringop-overflow */
  strncat (a4, "1", 4);
}

NOIPA void cat_a4_s1_5 (void)
{
  /* A bound in excess of the destination size is diagnosed by
     -Wstringop-overflow.  */
  strncat (a4, "1", 5);
}

NOIPA void cat_a4_s1_dlen (void)
{
  strncat (a4, "1", sizeof a4 - strlen (a4) - 1);
}

NOIPA void cat_a4_s2_dlen (void)
{
  strncat (a4, "12", sizeof a4 - strlen (a4) - 1);  /* { dg-bogus "\\\[-Wstringop-truncation]" } */
}

NOIPA void cat_a4_b4_dlen (void)
{
  strncat (a4, b4, sizeof a4 - strlen (a4) - 1);  /* { dg-bogus "\\\[-Wstringop-truncation]" } */
}

NOIPA void cat_ax_b4_dlen (void)
{
  strncat (ax, b4, 32 - strlen (ax) - 1);  /* { dg-bogus "\\\[-Wstringop-truncation]" } */
}
