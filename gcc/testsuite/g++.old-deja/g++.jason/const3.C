// { dg-do run  }
// Bug: bar isn't emitted, which causes havoc.

extern int i;
const int bar = i;
int i = 5;

int main()
{
  return bar != 5;
}
