// { dg-do compile }
// { dg-options "-O2" }

extern int foo (int);

void
bar (void)
{
  char tmp = foo (0);
  switch (tmp)
    {
    case 1: foo (1); break;
    case 2: foo (2); break;
    case 3: foo (3); break;
    case 4: foo (4); break;
    case 5: foo (5); break;
    case 6: foo (6); break;
    case 7: foo (7); break;
    case 255: foo (8); break;
    default: break;
    }
}

