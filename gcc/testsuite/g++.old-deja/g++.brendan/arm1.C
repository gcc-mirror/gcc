// Build don't link: 
// Special g++ Options: -pedantic-errors
// GROUPS passed ARM-compliance
// ARM $5.7, it's illegal to do math on a `void*'.

int
main()
{
  void *p;
  ++p;// ERROR - .*
}
