// { dg-do assemble  }

void foo ()
{
  int e;
  e := e;	// { dg-error "" } parse error
}
