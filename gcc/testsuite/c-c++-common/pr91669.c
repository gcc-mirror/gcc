/* { dg-do compile } */
/* { dg-additional-options "-Wreturn-type" } */

/* The location of the right brace within the macro expansion can be an adhoc
   location, because the frontend attached custom data to it.  In order for the
   diagnostic pragma to correctly understand that the diagnostic pop occurs
   after the function and not before, linemap_location_before_p needs to handle
   adhoc locations within a macro map, which was broken until fixed by r10-325.
   Verify that we get it right, both when the brace is a macro token and when it
   is part of the macro expansion.  */

#define ENDFUNC1 \
  _Pragma("GCC diagnostic push") \
  _Pragma("GCC diagnostic ignored \"-Wreturn-type\"") \
  } /* { dg-bogus {-Wreturn-type} } */ \
  _Pragma("GCC diagnostic pop")

int f1 () {
ENDFUNC1 /* { dg-bogus {in expansion of macro 'ENDFUNC1' } } */

#define ENDFUNC2(term) \
  _Pragma("GCC diagnostic push") \
  _Pragma("GCC diagnostic ignored \"-Wreturn-type\"") \
  term /* { dg-bogus {in definition of macro 'ENDFUNC2'} } */ \
  _Pragma("GCC diagnostic pop")

int f2 () {
ENDFUNC2(}) /* { dg-bogus {-Wreturn-type} } */
