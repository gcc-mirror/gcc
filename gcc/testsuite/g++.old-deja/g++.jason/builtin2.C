// Build don't link:

static inline void strlen (const char *) { }

void f ()
{
  strlen("Hi");			// gets bogus error - wrongful overload
}
