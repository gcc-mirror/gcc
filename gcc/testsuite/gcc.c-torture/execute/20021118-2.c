/* Originally added to test SH constant pool layout.  t1() failed for
   non-PIC and t2() failed for PIC.  */

int t1 (float *f, int i,
	void (*f1) (double),
	void (*f2) (float, float))
{
  f1 (3.0);
  f[i] = f[i + 1];
  f2 (2.5f, 3.5f);
}

int t2 (float *f, int i,
	void (*f1) (double),
	void (*f2) (float, float),
	void (*f3) (float))
{
  f3 (6.0f);
  f1 (3.0);
  f[i] = f[i + 1];
  f2 (2.5f, 3.5f);
}

void f1 (double d)
{
  if (d != 3.0)
    abort ();
}

void f2 (float f1, float f2)
{
  if (f1 != 2.5f || f2 != 3.5f)
    abort ();
}

void f3 (float f)
{
  if (f != 6.0f)
    abort ();
}

int main ()
{
  float f[3] = { 2.0f, 3.0f, 4.0f };
  t1 (f, 0, f1, f2);
  t2 (f, 1, f1, f2, f3);
  if (f[0] != 3.0f && f[1] != 4.0f)
    abort ();
  exit (0);
}
