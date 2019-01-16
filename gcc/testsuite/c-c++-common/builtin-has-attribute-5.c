/* Verify __builtin_has_attribute return value for attributes constructor
   and destructor with explicit priorities.
   { dg-do compile { target init_priority } }
   { dg-options "-Wall -ftrack-macro-expansion=0" }
   { dg-options "-Wall -Wno-narrowing -Wno-unused -ftrack-macro-expansion=0" { target c++ } }  */

#define ATTR(...) __attribute__ ((__VA_ARGS__))

#define A(expect, sym, attr)						\
  typedef int Assert [1 - 2 * !(__builtin_has_attribute (sym, attr) == expect)]

void fnone (void);

void test_ctor_dtor_prio (void)
{
  extern ATTR (constructor) void fctor (void);
  extern ATTR (destructor) void fdtor (void);
  extern ATTR (constructor, destructor) void fctor_dtor (void);

  A (0, fnone, constructor);
  A (0, fnone, constructor (123));
  A (0, fnone, destructor);
  A (0, fnone, constructor (234));

  A (1, fctor, constructor);
  A (0, fctor, constructor (123));
  A (1, fdtor, destructor);
  A (0, fdtor, destructor (234));

  extern ATTR (constructor) void fctor_dtor (void);
  extern ATTR (destructor) void fctor_dtor (void);
  extern ATTR (constructor, destructor) void fctor_dtor (void);

  A (1, fctor_dtor, constructor);
  A (1, fctor_dtor, destructor);

  extern ATTR (constructor (123)) void fctor_123 (void);
  A (1, fctor_123, constructor);
  A (0, fctor_123, destructor);
  A (1, fctor_123, constructor (123));
  A (0, fctor_123, constructor (124));

  extern ATTR (destructor (234)) void fctor_123 (void);
  A (1, fctor_123, constructor (123));
  A (1, fctor_123, destructor);
  A (1, fctor_123, destructor (234));
  A (0, fctor_123, destructor (235));
}
