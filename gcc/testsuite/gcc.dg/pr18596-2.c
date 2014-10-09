/* { dg-do compile } */
/* { dg-options "-funit-at-a-time -std=gnu89" } */

int f(int i)
{
  static int g(); /* { dg-error "invalid storage class" } */
  static int g() { return i; } /* { dg-error "invalid storage class" } */
  return g();
}

int k (int i)
{
  static int g (); /* { dg-error "invalid storage class" } */
  int g () {
	return i;
  }

  return g ();
}

int l (int i)
{
  auto int g ();
  static int g () { /* { dg-error "invalid storage class" } */
    return i;
  }

  static int h () { /* { dg-error "invalid storage class" } */
    return 3;
  }
  return g () + h ();
}

int m (int i)
{
  static g ();  /* { dg-error "invalid storage class" } */
  static g () { return i; } /* { dg-error "invalid storage class" } */
  return g ();
}
