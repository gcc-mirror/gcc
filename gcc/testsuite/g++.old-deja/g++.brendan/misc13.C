// Build don't link: 
// GROUPS passed miscellaneous-bugs
// This should only give warnings from duplicate_decls; it should not get
// errors from push_overloaded_decl as well.

extern "C"
{
  long unsigned int strlen(char*);// ERROR -  warning// ERROR -  warning.*
}
