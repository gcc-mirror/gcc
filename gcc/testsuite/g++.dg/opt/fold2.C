// PR optimization/14669
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort (void);
extern "C" void exit (int);

enum ActionType { EE=-1, E0=0, E1, E2 };

int main(void)
{
  ActionType t = E0;

  if (E1 <= t && t <= E2)
    abort ();

  exit (0);
}

