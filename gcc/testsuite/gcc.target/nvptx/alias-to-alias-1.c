/* Alias to alias; 'libgomp.c-c++-common/pr96390.c'.  */

/* { dg-do link } */
/* { dg-do run { target nvptx_runtime_alias_ptx } } */
/* { dg-options -save-temps } */
/* { dg-add-options nvptx_alias_ptx } */

int v;

void foo () { v = 42; }
void bar () __attribute__((alias ("foo")));
void baz () __attribute__((alias ("bar")));

int
main (void)
{
  baz ();
  if (v != 42)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-assembler-times {(?n)^// BEGIN GLOBAL FUNCTION DECL: foo$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.visible \.func foo;$} 1 } }
   { dg-final { scan-assembler-times {(?n)^// BEGIN GLOBAL FUNCTION DEF: foo$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.visible \.func foo$} 1 } } */

/* { dg-final { scan-assembler-times {(?n)^// BEGIN GLOBAL FUNCTION DECL: bar$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.visible \.func bar;$} 1 } }
   { dg-final { scan-assembler-times {(?n)^// BEGIN GLOBAL FUNCTION DEF: bar$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.alias bar,foo;$} 1 } } */

/* { dg-final { scan-assembler-times {(?n)^// BEGIN GLOBAL FUNCTION DECL: baz$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.visible \.func baz;$} 1 } }
   { dg-final { scan-assembler-times {(?n)^// BEGIN GLOBAL FUNCTION DEF: baz$} 1 } }
   { dg-final { scan-assembler-times {(?n)^\.alias baz,foo;$} 1 } } */

/* { dg-final { scan-assembler-times {(?n)\tcall foo;$} 0 } }
   { dg-final { scan-assembler-times {(?n)\tcall bar;$} 0 } }
   { dg-final { scan-assembler-times {(?n)\tcall baz;$} 1 } } */
