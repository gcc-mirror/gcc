// Build don't link:

inline void strlen (const char *) { }

void f ()
{
  strlen("Hi");			// gets bogus error - wrongful overload
}
