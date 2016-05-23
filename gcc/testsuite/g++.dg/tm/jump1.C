// A goto or switch statement shall not be used to transfer control into a
// synchronized or atomic block.
// { dg-options "-fgnu-tm" }

void f()
{
  static int i;
  synchronized {
    ++i;
  inside:			// { dg-message "" }
    ++i;
  }
  goto inside;			// { dg-message "" }

  switch (i)
    {
      synchronized {		// { dg-warning "statement will never be executed" }
	++i;
      case 42:			// { dg-error "" }
	++i;
      }
    }
}
