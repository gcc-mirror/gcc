// { dg-do assemble  }

int i;
int &const j = i;		// { dg-error "" } invalid const
int &const f();			// { dg-error "" } invalid const
void g ()
{
  j = 1;
  f() = 1;
}
