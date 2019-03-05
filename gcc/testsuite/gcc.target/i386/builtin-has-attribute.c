/* Verify __builtin_has_attribute return value for i386 function attributes.
   { dg-do compile }
   { dg-options "-Wall -ftrack-macro-expansion=0" }
   { dg-options "-Wall -Wno-narrowing -Wno-unused -ftrack-macro-expansion=0" { target c++ } }  */

#define ATTR(...) __attribute__ ((__VA_ARGS__))

#define A(expect, sym, attr)						\
  typedef int Assert [1 - 2 * !(__builtin_has_attribute (sym, attr) == expect)]

void fnone (void);


ATTR (interrupt) void finterrupt (void*);
ATTR (interrupt, naked) void fnaked_interrupt (void*);

A (0, fnone, interrupt);
A (1, finterrupt, interrupt);
A (1, fnaked_interrupt, interrupt);
A (1, fnaked_interrupt, naked);


ATTR (naked) void fnaked (void);

A (0, fnone, naked);
A (1, fnaked, naked);


ATTR (no_caller_saved_registers) void fnsr (int);

A (0, fnone, no_caller_saved_registers);
A (1, fnsr, no_caller_saved_registers);


ATTR (target ("abm")) void ftarget_abm (void);
ATTR (target ("mmx")) void ftarget_mmx (void);
ATTR (target ("mmx"), target ("sse")) void ftarget_mmx_sse (void);

A (0, fnone, target);
A (0, fnone, target ("abm"));
A (0, fnone, target ("mmx"));

A (1, ftarget_abm, target);
A (0, ftarget_abm, target ("no-abm"));
A (1, ftarget_abm, target ("abm"));

A (1, ftarget_mmx, target);
A (0, ftarget_mmx, target ("no-mmx"));
A (1, ftarget_mmx, target ("mmx"));

A (1, ftarget_mmx_sse, target);
A (0, ftarget_mmx_sse, target ("no-mmx"));
A (1, ftarget_mmx_sse, target ("mmx"));
A (1, ftarget_mmx_sse, target ("sse"));
