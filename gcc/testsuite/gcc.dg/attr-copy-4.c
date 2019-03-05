/* PR middle-end/81824 - Warn for missing attributes with function aliases
   Exercise attribute copy for types.
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

#define Assert(expr)   typedef char AssertExpr[2 * !!(expr) - 1]

#define ATTR(list)   __attribute__ (list)

/* Use attribute packed to verify that type attributes are copied
   from one type to another.  */

struct ATTR ((packed)) PackedA { int i; char c; };

Assert (__alignof (struct PackedA) == 1);

struct ATTR ((copy ((struct PackedA*)0))) PackedB { long i; char c; };

Assert (__alignof (struct PackedA) == __alignof (struct PackedB));

struct PackedMember
{
  char c;
  ATTR ((copy ((struct PackedB*)0))) double packed_mem;
};

Assert (__alignof (struct PackedMember) == 1);


extern const struct PackedA packed;

struct Unpacked { int i; char c; };
Assert (__alignof (struct Unpacked) > 1);

/* Verify that copying the packed attribute to the declaration
   of an object is ignored with a warning.  (There should be
   a way to copy just the subset of attributes from a type that
   aren't ignored and won't cause a warning, maybe via attribute
   copy_except or something like that.)  */
extern ATTR ((copy ((struct PackedA*)0))) const struct Unpacked
  unpacked;                   /* { dg-warning ".packed. attribute ignored" } */

Assert (__alignof (packed) == 1);
Assert (__alignof (unpacked) == __alignof (struct Unpacked));



/* Verify that attribute deprecated isn't copied (but referencing
   the deprecated type in the copy attribute still triggers a warning).  */

struct ATTR ((aligned (8), deprecated))
AlignedDeprecated { char c; };

struct ATTR ((copy ((struct AlignedDeprecated *)0)))        /* { dg-warning "\\\[-Wdeprecated-declarations]" } */
AlignedCopy { short s; };

Assert (__alignof (struct AlignedCopy) == 8);

struct AlignedCopy aligned_copy;

Assert (__alignof (aligned_copy) == 8);
