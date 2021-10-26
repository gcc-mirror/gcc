/* Verify that a separate note is issued for each offset into the same
   object after a -Wstringop-overflow.  Since all arguments are known
   the test doesn't need optimization.  Wstringop-overflow-79.c verifies
   they're also issued at -O2.
   { dg-do compile }
   { dg-options "-O0 -Wno-array-bounds" } */

extern char a[8];                 // dg-message at offset \\\[3, 6] into destination object 'a'" "note 1" }
                                  // dg-message at offset \\\[5, 8] into destination object 'a'" "note 2" { target *-*-* } .-1 }

void test_2_notes (int i)
{
  char *p = i ? a + 3 : a + 5;
  __builtin_memset (p, 0, 7);     // { dg-warning "-Wstringop-overflow" }
}


extern char b[8];                 // dg-message at offset \\\[3, 6] into destination object 'b'" "note 1" }
                                  // dg-message at offset \\\[4, 7] into destination object 'b'" "note 2" { target *-*-* } .-1 }
                                  // dg-message at offset \\\[5, 8] into destination object 'b'" "note 3" { target *-*-* } .-2 }

void test_3_notes (int i)
{
  char *p = i < 0 ? b + 3 : 0 < i ? b + 5 : b + 4;
  __builtin_memset (p, 0, 7);     // { dg-warning "-Wstringop-overflow" }
}


extern char c[8];                 // dg-message at offset \\\[3, 6] into destination object 'c'" "note 1" }
                                  // dg-message at offset \\\[4, 7] into destination object 'c'" "note 2" { target *-*-* } .-1 }
                                  // dg-message at offset \\\[5, 8] into destination object 'c'" "note 3" { target *-*-* } .-2 }
                                  // dg-message at offset \\\[6, 8] into destination object 'c'" "note 3" { target *-*-* } .-2 }

void test_4_notes (int i)
{
  char *p;
  if (i < -1)
    p = c + 3;
  else if (i < 0)
    p = c + 4;
  else if (0 < i)
    p = c + 6;
  else
    p = c + 5;

  __builtin_memset (p, 0, 7);     // { dg-warning "-Wstringop-overflow" }
}


extern char d[8];                 // dg-message at offset \\\[3, 6] into destination object 'd'" "note 1" }
                                  // dg-message at offset \\\[4, 7] into destination object 'd'" "note 2" { target *-*-* } .-1 }
                                  // dg-message at offset \\\[5, 8] into destination object 'd'" "note 3" { target *-*-* } .-2 }
                                  // dg-message at offset \\\[6, 8] into destination object 'd'" "note 3" { target *-*-* } .-3 }
                                  // dg-message at offset \\\[7, 8] into destination object 'd'" "note 3" { target *-*-* } .-4 }

void test_5_notes (int i)
{
  char *p;
  switch (i)
    {
    case -9: p = d + 3; break;
    case -5: p = d + 4; break;
    case  0: p = d + 5; break;
    case  3: p = d + 6; break;
    case  4: p = d + 7; break;
    default: return;
    }

  __builtin_memset (p, 0, 7);     // { dg-warning "-Wstringop-overflow" }
}
