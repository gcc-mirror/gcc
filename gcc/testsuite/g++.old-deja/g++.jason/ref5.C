// Build don't link:
// Special g++ Options: -w
int i;
int &const j = i;
int &const f();
void g ()
{
  j = 1;
  f() = 1;
}
