// PR ipa/65765
// { dg-do run }
// { dg-options "-O2" }

int a, b, c, d, e;
unsigned char h[] = { 1, 1 };

__attribute__ ((cold)) int ModRM_Mode () { return a; }

int
ModRM_RM (int p1)
{
  return p1;
}

__attribute__ ((cold)) static bool ModRM_hasSIB (unsigned char p1)
{
  return ModRM_Mode () != 1 && ModRM_RM (p1);
}

__attribute__ ((cold)) static bool ModRM_hasRIP (unsigned char p1)
{
  return ModRM_Mode () && ModRM_RM (p1);
}

unsigned char *
DisassembleHeapAccess (unsigned char *p1)
{
  b = *p1++;
  if (ModRM_hasSIB (b))
    c = *p1++;
  int f = c, g = 0;
  d = ModRM_hasRIP (g);
  e = f == 0;
  if (e)
    p1 += sizeof 0;
  return p1;
}

int
main ()
{
  if (DisassembleHeapAccess (h) != h + 2)
    __builtin_abort ();
}
