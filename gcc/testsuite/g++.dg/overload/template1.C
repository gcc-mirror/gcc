template<typename T> T Foo (int) {T d;}

void Baz (void (*)(int), int);

int Foo ();
int Baz (int (*)(), float);

void Bar ()
{
  Baz (Foo, 1.0f);
  
}
