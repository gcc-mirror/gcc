// { dg-do run  }
// GROUPS passed niklas hiding local-types
extern "C" int printf (const char*, ...);
int val = 1;
void S () { printf ("FAIL\n"); }
void f () { printf ("PASS\n"); val = 0; }
int main ()
{
  struct S { S () { f (); } };
  S ();
  return val;
}
