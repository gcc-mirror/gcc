/* PR middle-end/97879 - ICE on invalid mode in attribute access
   { dg-do compile }
   { dg-options "-Wall" } */

#define A(...) __attribute__ ((access (__VA_ARGS__)))

A (" ", 1) void f1 (int *);       // { dg-error "attribute 'access' mode '\" \"' is not an identifier; expected one of 'read_only', 'read_write', 'write_only', or 'none'" }
           void f1 (int *);


A ("none", 1) void f2 (char *);   // { dg-error "not an identifier" }
              void f2 (char *);

A (1) void f3 ();                 // { dg-error "not an identifier" }

A (1, 2) void f4 ();              // { dg-error "not an identifier" }
A (2., 3.) void f5 ();            // { dg-error "not an identifier" }

// Verify that copying a valid access attribute doesn't cause errors.
A (read_only, 1, 2)         void f6 (void*, int);
__attribute__ ((copy (f6))) void f7 (void*, int);
