// { dg-do assemble  }
// GROUPS passed warnings
void foo()
{
  int i;
  if (1) {
    for (int i = 0; i < 10; i++)
      ;
  }
}
