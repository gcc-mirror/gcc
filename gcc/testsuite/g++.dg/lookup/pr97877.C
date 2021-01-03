// PR 97877, duplicate decls smashed decl_lang_specific

void f ()
{
  extern int a;
  extern int a;
  a = 2;
}
