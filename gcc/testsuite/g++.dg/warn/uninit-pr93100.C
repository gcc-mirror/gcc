/* PR tree-optimization/98508 - Sanitizer disable -Wall and -Wextra
   { dg-do compile }
   { dg-options "-O0 -Wall -fsanitize=address" }
   { dg-skip-if "no address sanitizer" { no_fsanitize_address } } */

struct S
{
  int a;
};

void warn_init_self_O0 ()
{
  S s = S (s);      // { dg-warning "\\\[-Wuninitialized" }
  (void)&s;
}


void warn_init_self_use_O0 ()
{
  S s = S (s);      // { dg-warning "\\\[-Wuninitialized" }

  void sink (void*);
  sink (&s);
}


#pragma GCC optimize ("1")

void warn_init_self_O1 ()
{
  S s = S (s);      // { dg-warning "\\\[-Wuninitialized" }
  (void)&s;
}


void warn_init_self_use_O1 ()
{
  S s = S (s);      // { dg-warning "\\\[-Wuninitialized" }

  void sink (void*);
  sink (&s);
}


#pragma GCC optimize ("2")

void warn_init_self_O2 ()
{
  S s = S (s);      // { dg-warning "\\\[-Wuninitialized" }
  (void)&s;
}


void warn_init_self_use_O2 ()
{
  S s = S (s);      // { dg-warning "\\\[-Wuninitialized" }

  void sink (void*);
  sink (&s);
}
