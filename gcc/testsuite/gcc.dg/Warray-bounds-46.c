/* PR middle-end/91631 - buffer overflow into an array member of a declared
   object not detected
   Test to verify that past-the-end accesses by string functions to member
   arrays by-reference objects are diagnosed.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-unused-local-typedefs -Wno-stringop-overflow -ftrack-macro-expansion=0" }
   { dg-require-effective-target alloca } */

#define SA(expr) typedef int StaticAssert [2 * !!(expr) - 1]

typedef __SIZE_TYPE__ size_t;

extern char* strcpy (char*, const char*);
extern char* strncpy (char*, const char*, size_t);

void sink (void*);

struct MA17
{
  char pad[4];
  char a1[1], a2[2], a3[3], a4[4], a5[5], a6[6], a7[7], a8[8], a9[9], a10[10];
  char a11[11], a12[12], a13[13], a14[14], a15[15], a16[16], a17[17], ax[];
};

extern struct MA17 gma;
extern struct MA17 gma2[2];

struct MA17 igma_3 = { .ax = { 1, 2, 3 } };
struct MA17 igma2_[2];

#define S36 "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#define S(N)   (S36 + sizeof (S36) - N - 1)

/* In the test macro, prevent the strcpy to memcpy transformation
   by using a local array initialized with the string literal.  Without
   it, GCC transforms the strcpy call with memcpy which (unfortunately)
   permits accesses that cross subobject boundaries.  */
#define T(dst, n)				\
  do {						\
    const char a[] = S36;			\
    strcpy (dst, a + sizeof a - n - 1);		\
    sink (dst);					\
  } while (0)

void strcpy_global (void)
{
  T (gma.a1, 0);
  T (gma.a1, 1);          // { dg-warning "'strcpy' offset 5 from the object at 'gma' is out of the bounds of referenced subobject 'a1' with type 'char\\\[1]' at offset 4" }
  T (gma.a1, 4);          // { dg-warning "'strcpy' offset \\\[5, 8] from the object at 'gma' is out of the bounds of referenced subobject 'a1' with type 'char\\\[1]' at offset 4" }

  T (gma.a2, 1);
  T (gma.a2, 2);          // { dg-warning "'strcpy' offset 7 from the object at 'gma' is out of the bounds of referenced subobject 'a2' with type 'char\\\[2]' at offset 5" }

  T (gma.a3, 2);
  T (gma.a3, 3);          // { dg-warning "'strcpy' offset 10 from the object at 'gma' is out of the bounds of referenced subobject 'a3' with type 'char\\\[3]' at offset 7" }

  T (gma.a4, 3);
  T (gma.a4, 4);          // { dg-warning "'strcpy' offset 14 from the object at 'gma' is out of the bounds of referenced subobject 'a4' with type 'char\\\[4]' at offset 10" }

  T (gma.a5, 4);
  T (gma.a5, 5);          // { dg-warning "'strcpy' offset 19 from the object at 'gma' is out of the bounds of referenced subobject 'a5' with type 'char\\\[5]' at offset 14" }

  SA (__builtin_offsetof (struct MA17, a17) == 140);

  T (gma.a17, 16);
  T (gma.a17, 17);        // { dg-warning "'strcpy' offset 157 from the object at 'gma' is out of the bounds of referenced subobject 'a17' with type 'char\\\[17]' at offset 140" }

  SA (__builtin_offsetof (struct MA17, ax) == 157);
  // GCC allows static initialization of flexible array members of
  // non-local objects.  Verify that writing into one that may be
  // initialized in another translation unit isn't diagnosed.  */
  T (gma.ax, 0);          // { dg-bogus "\\\[-Warray-bounds" }
}


void strcpy_global_array (void)
{
  T (gma2[0].a1, 0);
  T (gma2[0].a1, 1);      // { dg-warning "'strcpy' offset 5 from the object at 'gma2' is out of the bounds of referenced subobject 'a1' with type 'char\\\[1]' at offset 4" }
  T (gma2[0].a1, 4);      // { dg-warning "'strcpy' offset \\\[5, 8] from the object at 'gma2' is out of the bounds of referenced subobject 'a1' with type 'char\\\[1]' at offset 4" }

  T (gma2[0].a2, 1);
  T (gma2[0].a2, 2);      // { dg-warning "'strcpy' offset 7 from the object at 'gma2' is out of the bounds of referenced subobject 'a2' with type 'char\\\[2]' at offset 5" }

  T (gma2[0].a3, 2);
  T (gma2[0].a3, 3);      // { dg-warning "'strcpy' offset 10 from the object at 'gma2' is out of the bounds of referenced subobject 'a3' with type 'char\\\[3]' at offset 7" }

  T (gma2[0].a4, 3);
  T (gma2[0].a4, 4);      // { dg-warning "'strcpy' offset 14 from the object at 'gma2' is out of the bounds of referenced subobject 'a4' with type 'char\\\[4]' at offset 10" }

  T (gma2[0].a5, 4);
  T (gma2[0].a5, 5);      // { dg-warning "'strcpy' offset 19 from the object at 'gma2' is out of the bounds of referenced subobject 'a5' with type 'char\\\[5]' at offset 14" }

  T (gma2[0].a17, 16);
  T (gma2[0].a17, 17);    // { dg-warning "'strcpy' offset 157 from the object at 'gma2' is out of the bounds of referenced subobject 'a17' with type 'char\\\[17]' at offset 140" }

  /* GMA2 is external but because it's an array its definition in another
     translation unit may not provide an initializer for the flexible array
     member.  Verify that a warning is issued for access to it.  */
  T (gma2[0].ax, 1);      // { dg-warning "'strcpy' offset \\\[157, 158] from the object at 'gma2' is out of the bounds of referenced subobject 'ax' with type 'char\\\[]' at offset 157" }
  T (gma2[0].ax, 7);      // { dg-warning "'strcpy' offset \\\[157, 164] from the object at 'gma2' is out of the bounds of referenced subobject 'ax' with type 'char\\\[]' at offset 157" }

  /* IGMA2_ is internal and provides no definition for the flexible array
     member.  Verify that a warning is issued for out-of-bounds accesses
     to it.  */
  T (igma2_[0].ax, 1);    // { dg-warning "'strcpy' offset \\\[157, 158] from the object at 'igma2_' is out of the bounds of referenced subobject 'ax' with type 'char\\\[]' at offset 157" }

  T (igma_3.ax, 0);
  T (igma_3.ax, 1);
  T (igma_3.ax, 1);
  T (igma_3.ax, 3);       // { dg-warning " offset 160 " }
  T (igma_3.ax, 9);       // { dg-warning " offset \\\[160, 166] " }
}


void strcpy_local (void)
{
  struct MA17 lma;

  T (lma.a1, 0);
  T (lma.a1, 1);          // { dg-warning "'strcpy' offset 5 from the object at 'lma' is out of the bounds of referenced subobject 'a1' with type 'char\\\[1]' at offset 4" }
  T (lma.a1, 4);          // { dg-warning "'strcpy' offset \\\[5, 8] from the object at 'lma' is out of the bounds of referenced subobject 'a1' with type 'char\\\[1]' at offset 4" }

  T (lma.a2, 1);
  T (lma.a2, 2);          // { dg-warning "'strcpy' offset 7 from the object at 'lma' is out of the bounds of referenced subobject 'a2' with type 'char\\\[2]' at offset 5" }

  T (lma.a3, 2);
  T (lma.a3, 3);          // { dg-warning "'strcpy' offset 10 from the object at 'lma' is out of the bounds of referenced subobject 'a3' with type 'char\\\[3]' at offset 7" }

  T (lma.a4, 3);
  T (lma.a4, 4);          // { dg-warning "'strcpy' offset 14 from the object at 'lma' is out of the bounds of referenced subobject 'a4' with type 'char\\\[4]' at offset 10" }

  T (lma.a5, 4);
  T (lma.a5, 5);          // { dg-warning "'strcpy' offset 19 from the object at 'lma' is out of the bounds of referenced subobject 'a5' with type 'char\\\[5]' at offset 14" }

  T (lma.a17, 16);
  T (lma.a17, 17);        // { dg-warning "'strcpy' offset 157 from the object at 'lma' is out of the bounds of referenced subobject 'a17' with type 'char\\\[17]' at offset 140" }

  T (lma.ax, 0);          // { dg-warning "'strcpy' offset 157 from the object at 'lma' is out of the bounds of referenced subobject 'ax' with type 'char\\\[]' at offset 157" }
}


void strcpy_ref (struct MA17 *pma)
{
  T (pma->a1, 0);
  T (pma->a1, 1);         // { dg-warning "'strcpy' offset 5 from the object at 'pma' is out of the bounds of referenced subobject 'a1' with type 'char\\\[1]' at offset 4" }
  T (pma->a1, 4);         // { dg-warning "'strcpy' offset \\\[5, 8] from the object at 'pma' is out of the bounds of referenced subobject 'a1' with type 'char\\\[1]' at offset 4" }

  T (pma->a2, 1);
  T (pma->a2, 2);         // { dg-warning "'strcpy' offset 7 from the object at 'pma' is out of the bounds of referenced subobject 'a2' with type 'char\\\[2]' at offset 5" }

  T (pma->a3, 2);
  T (pma->a3, 3);         // { dg-warning "'strcpy' offset 10 from the object at 'pma' is out of the bounds of referenced subobject 'a3' with type 'char\\\[3]' at offset 7" }

  T (pma->a4, 3);
  T (pma->a4, 4);         // { dg-warning "'strcpy' offset 14 from the object at 'pma' is out of the bounds of referenced subobject 'a4' with type 'char\\\[4]' at offset 10" }

  T (pma->a5, 4);
  T (pma->a5, 5);         // { dg-warning "'strcpy' offset 19 from the object at 'pma' is out of the bounds of referenced subobject 'a5' with type 'char\\\[5]' at offset 14" }

  T (pma->a17, 16);
  T (pma->a17, 17);       // { dg-warning "'strcpy' offset 157 from the object at 'pma' is out of the bounds of referenced subobject 'a17' with type 'char\\\[17]' at offset 140" }

  T (pma->ax, 0);
  T ((*pma).ax, 8);
  T (pma[0].ax, 9);

  SA (__builtin_offsetof (struct MA17, a1) == 4
      && sizeof (struct MA17) == 157);

  T (pma[1].a1, 0);
  T (pma[1].a1, 1);       // { dg-warning "'strcpy' offset 162 from the object at 'pma' is out of the bounds of referenced subobject 'a1' with type 'char\\\[1]' at offset 161" }
  T (pma[1].a1, 4);       // { dg-warning "'strcpy' offset \\\[162, 165] from the object at 'pma' is out of the bounds of referenced subobject 'a1' with type 'char\\\[1]' at offset 161" }

  T (pma[1].a2, 1);
  T (pma[1].a2, 2);       // { dg-warning "'strcpy' offset 164 from the object at 'pma' is out of the bounds of referenced subobject 'a2' with type 'char\\\[2]' at offset 162" }

  T (pma[1].a3, 2);
  T (pma[1].a3, 3);       // { dg-warning "'strcpy' offset 167 from the object at 'pma' is out of the bounds of referenced subobject 'a3' with type 'char\\\[3]' at offset 164" }

  T (pma[1].a4, 3);
  T (pma[1].a4, 4);       // { dg-warning "'strcpy' offset 171 from the object at 'pma' is out of the bounds of referenced subobject 'a4' with type 'char\\\[4]' at offset 167" }

  T (pma[1].a5, 4);
  T (pma[1].a5, 5);       // { dg-warning "'strcpy' offset 176 from the object at 'pma' is out of the bounds of referenced subobject 'a5' with type 'char\\\[5]' at offset 171" }

  T (pma[1].a17, 16);
  T (pma[1].a17, 17);     // { dg-warning "'strcpy' offset 314 from the object at 'pma' is out of the bounds of referenced subobject 'a17' with type 'char\\\[17]' at offset 297" }

  /* Since PMA points to an array of structs, accessing the flexible
     member of any of the elements of the array except for the last one
     would necessarily access a part of the next element of the enclosing
     array.  The warning assumes that PMA doesn't point to the last element
     of the array which could in theory have nonzero elements without
     overlapping other objects.  */
  T (pma[1].ax, 0);       // { dg-warning "'strcpy' offset 314 from the object at 'pma' is out of the bounds of referenced subobject 'ax' with type 'char\\\[]' at offset 314" }
  T ((pma + 1)->ax, 1);   // { dg-warning "'strcpy' offset \\\[314, 315] from the object at 'pma' is out of the bounds of referenced subobject 'ax' with type 'char\\\[]' at offset 314" }
  T ((pma + 1)[1].ax, 2); // { dg-warning "'strcpy' offset \\\[471, 473] from the object at 'pma' is out of the bounds of referenced subobject 'ax' with type 'char\\\[]' at offset 471" }
  T ((*(pma + 2)).ax, 2); // { dg-warning "'strcpy' offset \\\[471, 473] from the object at 'pma' is out of the bounds of referenced subobject 'ax' with type 'char\\\[]' at offset 471" }
  T (pma[3].ax, 9);       // { dg-warning "'strcpy' offset \\\[628, 637] from the object at 'pma' is out of the bounds of referenced subobject 'ax' with type 'char\\\[]' at offset 628" }

  T (pma[-1].a1, 0);
  T (pma[-1].a1, 1);      // { dg-warning "'strcpy' offset -152 from the object at 'pma' is out of the bounds of referenced subobject 'a1' with type 'char\\\[1]' at offset -153" }
  T (pma[-1].a1, 4);      // { dg-warning "'strcpy' offset \\\[-152, -149] from the object at 'pma' is out of the bounds of referenced subobject 'a1' with type 'char\\\[1]' at offset -153" }
}

struct MA3
{
  char a4[4];             // { dg-message "'a4' declared here" }
  char a3[3];             // { dg-message "'a3' declared here" }
  char c;
};

void strcpy_ref_note (struct MA17 *pma, struct MA3 *pma3)
{
  T (pma3[-1].a4, 0);
  T (pma3[-1].a4, 1);
  T (pma3[-1].a4, 2);
  T (pma3[-1].a4, 3);
  T (pma3[-1].a4, 4);     // { dg-warning "'strcpy' offset -4 from the object at 'pma3' is out of the bounds of referenced subobject 'a4' with type 'char\\\[4]' at offset -8" }
  T (pma3[-1].a4, 5);     // { dg-warning "'strcpy' offset \\\[-4, -3] from the object at 'pma3' is out of the bounds of referenced subobject 'a4' with type 'char\\\[4]' at offset -8" }

  T (pma3[-1].a3, 0);
  T (pma3[-1].a3, 1);
  T (pma3[-1].a3, 2);
  T (pma3[-1].a3, 3);     // { dg-warning "'strcpy' offset -1 from the object at 'pma3' is out of the bounds of referenced subobject 'a3' with type 'char\\\[3]' at offset -4" }
  T (pma3[-1].a3, 4);     // { dg-warning "'strcpy' offset \\\[-1, 0] from the object at 'pma3' is out of the bounds of referenced subobject 'a3' with type 'char\\\[3]' at offset -4" }
}


void strncpy_vla_member (unsigned n)
{
  struct VarLenStruct {
    char a4[4], an[n], bn[n];
  } x;

  sink (&x);

  strncpy (x.bn, x.a4, sizeof x.bn);
  sink (&x);

  strncpy (x.a4, x.bn, sizeof x.a4);
  x.a4[sizeof x.a4 - 1] = '\0';
  sink (&x);

  strncpy (x.a4, x.bn, n);
  sink (&x);

  strncpy (x.an, x.bn, sizeof x.bn);    /* { dg-bogus "\\\[-Warray-bounds" } */
  sink (&x);
}
