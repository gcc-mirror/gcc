// { dg-do link }
// { dg-options "-O1 -finline-functions" }

static void g ();

void f()
{
  void g();
  g();
}

void g()
{
}

int main () {
  f ();
}
