/* PR middle-end/81824 - Warn for missing attributes with function aliases
   { dg-do compile }
   { dg-require-ifunc "" }
   { dg-options "-Wall -Wattribute-alias=2" } */

#define ATTR(...)   __attribute__ ((__VA_ARGS__))


void
target_no_nothrow (void)        /* { dg-message ".alias_nothrow. target declared here" } */
{ }

ATTR (alias ("target_no_nothrow"), nothrow) void
alias_nothrow (void);           /* { dg-warning ".alias_nothrow. specifies more restrictive attribute than its target .target_no_nothrow.: .nothrow." } */


#pragma GCC diagnostic push "-Wattribute-alias"
#pragma GCC diagnostic ignored "-Wattribute-alias"
ATTR (alias ("target_no_nothrow"), nothrow) void
alias_nothrow_ignored (void);
#pragma GCC diagnostic pop "-Wattribute-alias"


ATTR (pure) int
alias_pure (void);

int
target_no_pure (void)           /* { dg-message ".alias_pure. target declared here" } */
{ return 0; }

ATTR (alias ("target_no_pure")) int
alias_pure (void);              /* { dg-warning ".alias_pure. specifies more restrictive attribute than its target .target_no_pure.: .pure." } */


ATTR (const) int
alias_const (void);

int
target_pure (void)              /* { dg-message ".alias_const. target declared here" } */
{ return 0; }

ATTR (alias ("target_pure")) int
alias_const (void);             /* { dg-warning ".alias_const. specifies more restrictive attribute than its target .target_pure.: .const." } */


/* There is no obvious relationship between the attributes on an ifunc
   resolver and those on its aliases.  Verify that mismatches between
   aliases and ifunc resolvers do not trigger warnings.  */

typedef int F (void);

ATTR (pure, leaf) F* resolve_to_const (void)
{ return alias_const; }

ATTR (ifunc ("resolve_to_const")) F alias_no_const_ifunc;
ATTR (const, ifunc ("resolve_to_const")) F alias_const_ifunc;
ATTR (ifunc ("resolve_to_const")) int alias_no_leaf_ifunc (void);
