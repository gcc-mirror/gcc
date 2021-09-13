/* { dg-options "-fplugin-arg-location_overflow_plugin-value=0x60000001" } */

/* We use location_overflow_plugin.c, which injects the case that location_t
   values have exceeded LINE_MAP_MAX_LOCATION_WITH_COLS, and hence no column
   numbers are available.  */

/* Verify that we're in column-less mode.  */
extern unknown_type test; /* { dg-error "-:'unknown_type' does not name a type" } */

#define CONST const
#define VOID void
typedef CONST VOID *PCVOID;
