template <class T> void f (T);

void g ();
void g (int);

int
main ()
{
  f (g);			// ERROR - ambiguous unification
  return 0;
}
