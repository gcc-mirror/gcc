/* PR tree-optimization/48809 */

extern void abort (void);

int
foo (signed char x)
{
  int y = 0;
  switch (x)
    {
    case 0: y = 1; break;
    case 1: y = 7; break;
    case 2: y = 2; break;
    case 3: y = 19; break;
    case 4: y = 5; break;
    case 5: y = 17; break;
    case 6: y = 31; break;
    case 7: y = 8; break;
    case 8: y = 28; break;
    case 9: y = 16; break;
    case 10: y = 31; break;
    case 11: y = 12; break;
    case 12: y = 15; break;
    case 13: y = 111; break;
    case 14: y = 17; break;
    case 15: y = 10; break;
    case 16: y = 31; break;
    case 17: y = 7; break;
    case 18: y = 2; break;
    case 19: y = 19; break;
    case 20: y = 5; break;
    case 21: y = 107; break;
    case 22: y = 31; break;
    case 23: y = 8; break;
    case 24: y = 28; break;
    case 25: y = 106; break;
    case 26: y = 31; break;
    case 27: y = 102; break;
    case 28: y = 105; break;
    case 29: y = 111; break;
    case 30: y = 17; break;
    case 31: y = 10; break;
    case 32: y = 31; break;
    case 98: y = 18; break;
    case -62: y = 19; break;
    }
  return y;
}

int
main ()
{
  if (foo (98) != 18 || foo (97) != 0 || foo (99) != 0)
    abort ();
  if (foo (-62) != 19 || foo (-63) != 0 || foo (-61) != 0)
    abort ();
  if (foo (28) != 105 || foo (27) != 102 || foo (29) != 111)
    abort ();
  return 0;
}
