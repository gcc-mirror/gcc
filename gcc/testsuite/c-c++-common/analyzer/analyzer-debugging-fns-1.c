/* Various wrong declarations for the decls
   in analyzer-decls.h.

   Make sure we don't ICE on these.  */

extern void __analyzer_dump_capacity (int);

void wrong_analyzer_dump_capacity (void)
{
  __analyzer_dump_capacity (42);
}
