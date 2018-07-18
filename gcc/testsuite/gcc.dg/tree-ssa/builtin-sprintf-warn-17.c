/* PR tree-optimization/80497 - ICE at -O1 and above on valid code on
   x86_64-linux-gnu in "tree_to_uhwi"
   { dg-do compile }
   { dg-options "-O2 -Wall -Wformat-overflow" }
   { dg-require-effective-target int128 } */

extern char buf[];

const __int128_t sint128_max
  = (__int128_t)1 << (sizeof sint128_max * __CHAR_BIT__ - 2);

void fn0 (void)
{
  __int128_t si128 = 0;

  __builtin_sprintf (buf, "%*i", si128, 0);

  __builtin_sprintf (buf, "%.*i", si128, 0);

  __builtin_sprintf (buf, "%i", si128);

  __builtin_sprintf (buf, "%2$*1$i", si128, 0);

  __builtin_sprintf (buf, "%2$.*1$i", si128, 0);
}

void fn1 (void)
{
  __int128_t si128 = sint128_max;

  __builtin_sprintf (buf, "%*i", si128, 0);

  __builtin_sprintf (buf, "%.*i", si128, 0);

  __builtin_sprintf (buf, "%i", si128);

  __builtin_sprintf (buf, "%2$*1$i", si128, 0);

  __builtin_sprintf (buf, "%2$.*1$i", si128, 0);
}

/* { dg-prune-output "expects argument of type .int." } */
