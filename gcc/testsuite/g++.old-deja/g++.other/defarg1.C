// Build don't link:

int f (int x)
{
  extern void g (int i = f (x)); // ERROR - default argument uses local
  
  g();

  return 0;
}

int f (void);

int h1 (int (*)(int) = f);
int h2 (int (*)(double) = f); // ERROR - no matching f

template <class T>
int j (T t)
{
  extern void k (int i = j (t)); // ERROR - default argument uses local

  k ();

  return 0;
}

template int j (double);

