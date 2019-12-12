/* PR c/80619 - bad fix-it hint for GCC %lu directive with int argument: %wu
   { dg-do compile }
   { dg-options "-Wall -fdiagnostics-show-caret" } */

void T (const char*, ...) __attribute__ ((format (__gcc_diag__, 1, 2)));

void test_suggested_modifier (void)
{
  T ("%ld", 0);     // { dg-warning "format '%ld' expects argument of type 'long int', but argument 2 has type 'int'" }
  /* { dg-begin-multiline-output "" }
   T ("%ld", 0);
       ~~^   ~
         |   |
         |   int
         long int
       %d
   { dg-end-multiline-output "" } */

  T ("%li", 0);     // { dg-warning "format '%li' expects argument of type 'long int', but argument 2 has type 'int'" }
  /* { dg-begin-multiline-output "" }
   T ("%li", 0);
       ~~^   ~
         |   |
         |   int
         long int
       %i
       { dg-end-multiline-output "" } */

  T ("%lu", 0);     // { dg-warning "format '%lu' expects argument of type 'long unsigned int', but argument 2 has type 'int'" }
  /* { dg-begin-multiline-output "" }
   T ("%lu", 0);
       ~~^   ~
         |   |
         |   int
         long unsigned int
       %u
       { dg-end-multiline-output "" } */

  T ("%lx", 0);     // { dg-warning "format '%lx' expects argument of type 'long unsigned int', but argument 2 has type 'int'" }
  /* { dg-begin-multiline-output "" }
   T ("%lx", 0);
       ~~^   ~
         |   |
         |   int
         long unsigned int
       %x
       { dg-end-multiline-output "" } */

  T ("%lli", 0);    // { dg-warning "format '%lli' expects argument of type 'long long int', but argument 2 has type 'int'" }
  /* { dg-begin-multiline-output "" }
   T ("%lli", 0);
       ~~~^   ~
          |   |
          |   int
          long long int
       %i
       { dg-end-multiline-output "" } */

  T ("%llo", 0);    // { dg-warning "format '%llo' expects argument of type 'long long unsigned int', but argument 2 has type 'int'" }
  /* { dg-begin-multiline-output "" }
   T ("%llo", 0);
       ~~~^   ~
          |   |
          |   int
          long long unsigned int
       %o
       { dg-end-multiline-output "" } */

  T ("%llu", 0);    // { dg-warning "format '%llu' expects argument of type 'long long unsigned int', but argument 2 has type 'int'" }
  /* { dg-begin-multiline-output "" }
   T ("%llu", 0);
       ~~~^   ~
          |   |
          |   int
          long long unsigned int
       %u
       { dg-end-multiline-output "" } */

  T ("%llx", 0);    // { dg-warning "format '%llx' expects argument of type 'long long unsigned int', but argument 2 has type 'int'" }
  /* { dg-begin-multiline-output "" }
   T ("%llx", 0);
       ~~~^   ~
          |   |
          |   int
          long long unsigned int
       %x
       { dg-end-multiline-output "" } */
}

