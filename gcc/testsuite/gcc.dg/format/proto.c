/* PR c/93812 - ICE on redeclaration of an attribute format function without
   protoype
   It's not clear that attribute format should be accepted on functions
   without a prototype.  If it's decided that it shouldn't be the tests
   here will need to be adjusted.
   { dg-do compile }
   { dg-options "-std=gnu17 -Wall" } */

#define FMT(n1, n2) __attribute__((__format__(__printf__, n1, n2)))

// Exercise function declarations.
FMT (1, 2) void print1 ();

FMT (2, 3) void print2 ();
           void print2 ();

FMT (3, 4) void print3 ();
FMT (3, 4) void print3 ();

FMT (1, 2) void print4 ();
           void print4 (void);              // { dg-warning "'format' attribute can only be applied to variadic functions" }

           void print5 ();
FMT (1, 2) void print5 (void);              // { dg-warning "\\\[-Wattributes" }

FMT (1, 2) void print6 ();
          void print6 (const char*, ...);   // { dg-error "conflicting types" }

           void print7 (const char*, ...);
FMT (1, 2) void print7 ();                  // { dg-error "conflicting types" }


// Exercise function calls.
void test_print (void)
{
  print1 ("%i %s", 123, "");
  print1 ("%s %i", 123, 123);               // { dg-warning "\\\[-Wformat" }

  print2 (0, "%s %i", "", 123);
  print2 (1, "%i %s", "", 123);             // { dg-warning "\\\[-Wformat" }

  print3 (0, 1, "%s %i", "", 123);
  print3 (1, 2, "%i %s", "", 123);          // { dg-warning "\\\[-Wformat" }

  // Just verify there's no ICE.
  print4 ();
  print5 ();
  print6 ("%i %s", 123, "");
}


// Exercise declarations of pointers to functions.
FMT (1, 2) void (*pfprint1)();

FMT (2, 3) void (*pfprint2)();
           void (*pfprint2)();

FMT (3, 4) void (*pfprint3)();
FMT (3, 4) void (*pfprint3)();

FMT (1, 2) void (*pfprint4)();
           void (*pfprint4)(void);              // { dg-warning "'format' attribute can only be applied to variadic functions" }

           void (*pfprint5)();
FMT (1, 2) void (*pfprint5)(void);              // { dg-warning "\\\[-Wattributes" }

FMT (1, 2) void (*pfprint6)();
           void (*pfprint6)(const char*, ...);   // { dg-error "conflicting types" }

           void (*pfprint7)(const char*, ...);
FMT (1, 2) void (*pfprint7)();                  // { dg-error "conflicting types" }

// Exercise calls via function pointers.
void test_pfprint (void)
{
  pfprint1 ("%i %s", 123, "");
  pfprint1 ("%s %i", 123, 123);             // { dg-warning "\\\[-Wformat" }

  pfprint2 (0, "%s %i", "", 123);
  pfprint2 (1, "%i %s", "", 123);           // { dg-warning "\\\[-Wformat" }

  pfprint3 (0, 1, "%s %i", "", 123);
  pfprint3 (1, 2, "%i %s", "", 123);        // { dg-warning "\\\[-Wformat" }

  // Just verify there's no ICE.
  pfprint4 ();
  pfprint5 ();
  pfprint6 ("%i %s", 123, "");
}
