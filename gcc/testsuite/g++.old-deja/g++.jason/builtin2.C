// { dg-do assemble  }

static inline void strlen (const char *) { }

void f ()
{
  strlen("Hi");			// { dg-bogus "" } wrongful overload
}
