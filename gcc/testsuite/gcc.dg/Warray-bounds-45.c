/* PR middle-end/91631 - buffer overflow into an array member of a declared
   object not detected
   Test to verify that past-the-end accesses by string functions to member
   arrays by-reference objects are diagnosed.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-unused-local-typedefs -ftrack-macro-expansion=0" }  */

extern char* strcpy (char*, const char*);
extern char* strcat (char*, const char*);

void sink (void*, ...);

#define S36 "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#define S(N)   (S36 + sizeof (S36) - N - 1)

/* In the test macro, prevent the strcpy to memcpy transformation
   by using a local array initialized with the string literal.  Without
   it, GCC transforms the strcpy call with memcpy which (unfortunately)
   permits accesses that cross subobject boundaries.  */
#define T(dst, ncpy, ncat)			\
  do {						\
    const char a[] = S36;			\
    strcpy (dst, a + sizeof a - ncpy - 1);	\
    const char b[] = S36;			\
    strcat (dst, b + sizeof b - ncat - 1);	\
    sink (dst);					\
  } while (0)


struct MemArrays
{
  char a7[7];             // { dg-message "'a7' declared here" }
  char a4[4];             // { dg-message "'a4' declared here" }
  char a3[3];             // { dg-message "'a3' declared here" }
};

struct MemArrays gma;

void strcat_value (void)
{
  T (gma.a7, 1, 1);
  T (gma.a7, 1, 5);
  T (gma.a7, 1, 6);       // { dg-warning "'strcat' offset 7 from the object at 'gma' is out of the bounds of referenced subobject 'a7' with type 'char\\\[7]' at offset 0" }
  T (gma.a7, 1, 7);       // { dg-warning "'strcat' offset \\\[7, 8] from the object at 'gma' is out of the bounds of referenced subobject 'a7' with type 'char\\\[7]' at offset 0" }

  T (gma.a7, 2, 1);
  T (gma.a7, 2, 4);
  T (gma.a7, 2, 5);       // { dg-warning "'strcat' offset 7 from the object at 'gma' is out of the bounds of referenced subobject 'a7' with type 'char\\\[7]' at offset 0" }
  T (gma.a7, 2, 6);       // { dg-warning "'strcat' offset \\\[7, 8] from the object at 'gma' is out of the bounds of referenced subobject 'a7' with type 'char\\\[7]' at offset 0" }

  T (gma.a7, 5, 1);
  T (gma.a7, 5, 2);       // { dg-warning "'strcat' offset 7 from the object at 'gma' is out of the bounds of referenced subobject 'a7' with type 'char\\\[7]' at offset 0" }

  T (gma.a4, 1, 1);
  T (gma.a4, 1, 2);
  T (gma.a4, 1, 3);       // { dg-warning "'strcat' offset 11 from the object at 'gma' is out of the bounds of referenced subobject 'a4' with type 'char\\\[4]' at offset 7" }
   T (gma.a4, 1, 4);       // { dg-warning "'strcat' offset \\\[11, 12] from the object at 'gma' is out of the bounds of referenced subobject 'a4' with type 'char\\\[4]' at offset 7" }

  T (gma.a4, 2, 3);       // { dg-warning "'strcat' offset \\\[11, 12] from the object at 'gma' is out of the bounds of referenced subobject 'a4' with type 'char\\\[4]' at offset 7" }

  T (gma.a3, 1, 1);
  T (gma.a3, 1, 2);       // { dg-warning "'strcat' offset 14 from the object at 'gma' is out of the bounds of referenced subobject 'a3' with type 'char\\\[3]' at offset 11" }
}


void strcat_ref (struct MemArrays *pma)
{
  T (pma->a7, 1, 1);
  T (pma->a7, 1, 5);
  T (pma->a7, 1, 6);      // { dg-warning "'strcat' offset 7 from the object at 'pma' is out of the bounds of referenced subobject 'a7' with type 'char\\\[7]' at offset 0" }
  T (pma->a7, 1, 7);      // { dg-warning "'strcat' offset \\\[7, 8] from the object at 'pma' is out of the bounds of referenced subobject 'a7' with type 'char\\\[7]' at offset 0" }

  T (pma->a7, 2, 1);
  T (pma->a7, 2, 4);
  T (pma->a7, 2, 5);      // { dg-warning "'strcat' offset 7 from the object at 'pma' is out of the bounds of referenced subobject 'a7' with type 'char\\\[7]' at offset 0" }
  T (pma->a7, 2, 6);      // { dg-warning "'strcat' offset \\\[7, 8] from the object at 'pma' is out of the bounds of referenced subobject 'a7' with type 'char\\\[7]' at offset 0" }

  T (pma->a4, 1, 1);
  T (pma->a4, 1, 2);
  T (pma->a4, 1, 3);      // { dg-warning "'strcat' offset 11 from the object at 'pma' is out of the bounds of referenced subobject 'a4' with type 'char\\\[4]' at offset 7" }
   T (pma->a4, 1, 4);      // { dg-warning "'strcat' offset \\\[11, 12] from the object at 'pma' is out of the bounds of referenced subobject 'a4' with type 'char\\\[4]' at offset 7" }

  T (pma->a4, 2, 3);      // { dg-warning "'strcat' offset \\\[11, 12] from the object at 'pma' is out of the bounds of referenced subobject 'a4' with type 'char\\\[4]' at offset 7" }

  T (pma->a3, 1, 1);
  T (pma->a3, 1, 2);      // { dg-warning "'strcat' offset 14 from the object at 'pma' is out of the bounds of referenced subobject 'a3' with type 'char\\\[3]' at offset 11" }
}


#define T2(dst1, dst2, ncpy, ncat)		\
  do {						\
    const char a[] = S36;			\
    strcpy (dst1, a + sizeof a - ncpy - 1);	\
    const char b[] = S36;			\
    strcat (dst2, b + sizeof b - ncat - 1);	\
    sink (dst1, dst2);				\
  } while (0)

struct ArraysOfMemArrays
{
  struct MemArrays ma3[3];
} a3[3];

void strcat_arrays_of_arrays_value (void)
{
  T2 (a3[0].ma3[0].a7, a3[0].ma3[0].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
  T2 (a3[0].ma3[0].a7, a3[0].ma3[1].a7, 6, 6);
  T2 (a3[0].ma3[0].a7, a3[0].ma3[2].a7, 6, 6);

  T2 (a3[0].ma3[1].a7, a3[0].ma3[0].a7, 6, 6);
  T2 (a3[0].ma3[1].a7, a3[0].ma3[1].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
  T2 (a3[0].ma3[1].a7, a3[0].ma3[2].a7, 6, 6);

  T2 (a3[0].ma3[2].a7, a3[0].ma3[0].a7, 6, 6);
  T2 (a3[0].ma3[2].a7, a3[0].ma3[1].a7, 6, 6);
  T2 (a3[0].ma3[2].a7, a3[0].ma3[2].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }

  T2 (a3[0].ma3[0].a7, a3[1].ma3[0].a7, 6, 6);
  T2 (a3[0].ma3[0].a7, a3[1].ma3[1].a7, 6, 6);
  T2 (a3[0].ma3[0].a7, a3[1].ma3[2].a7, 6, 6);

  T2 (a3[0].ma3[1].a7, a3[1].ma3[0].a7, 6, 6);
  T2 (a3[0].ma3[1].a7, a3[1].ma3[1].a7, 6, 6);
  T2 (a3[0].ma3[1].a7, a3[1].ma3[2].a7, 6, 6);

  T2 (a3[0].ma3[2].a7, a3[1].ma3[0].a7, 6, 6);
  T2 (a3[0].ma3[2].a7, a3[1].ma3[1].a7, 6, 6);
  T2 (a3[0].ma3[2].a7, a3[1].ma3[2].a7, 6, 6);

  T2 (a3[0].ma3[0].a7, a3[2].ma3[0].a7, 6, 6);
  T2 (a3[0].ma3[0].a7, a3[2].ma3[1].a7, 6, 6);
  T2 (a3[0].ma3[0].a7, a3[2].ma3[2].a7, 6, 6);

  T2 (a3[0].ma3[1].a7, a3[2].ma3[0].a7, 6, 6);
  T2 (a3[0].ma3[1].a7, a3[2].ma3[1].a7, 6, 6);
  T2 (a3[0].ma3[1].a7, a3[2].ma3[2].a7, 6, 6);

  T2 (a3[0].ma3[2].a7, a3[2].ma3[0].a7, 6, 6);
  T2 (a3[0].ma3[2].a7, a3[2].ma3[1].a7, 6, 6);
  T2 (a3[0].ma3[2].a7, a3[2].ma3[2].a7, 6, 6);


  T2 (a3[1].ma3[0].a7, a3[0].ma3[0].a7, 6, 6);
  T2 (a3[1].ma3[0].a7, a3[0].ma3[1].a7, 6, 6);
  T2 (a3[1].ma3[0].a7, a3[0].ma3[2].a7, 6, 6);

  T2 (a3[1].ma3[1].a7, a3[0].ma3[0].a7, 6, 6);
  T2 (a3[1].ma3[1].a7, a3[0].ma3[1].a7, 6, 6);
  T2 (a3[1].ma3[1].a7, a3[0].ma3[2].a7, 6, 6);

  T2 (a3[1].ma3[2].a7, a3[0].ma3[0].a7, 6, 6);
  T2 (a3[1].ma3[2].a7, a3[0].ma3[1].a7, 6, 6);
  T2 (a3[1].ma3[2].a7, a3[0].ma3[2].a7, 6, 6);

  T2 (a3[1].ma3[0].a7, a3[1].ma3[0].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
  T2 (a3[1].ma3[0].a7, a3[1].ma3[1].a7, 6, 6);
  T2 (a3[1].ma3[0].a7, a3[1].ma3[2].a7, 6, 6);

  T2 (a3[1].ma3[1].a7, a3[1].ma3[0].a7, 6, 6);
  T2 (a3[1].ma3[1].a7, a3[1].ma3[1].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
  T2 (a3[1].ma3[1].a7, a3[1].ma3[2].a7, 6, 6);

  T2 (a3[1].ma3[2].a7, a3[1].ma3[0].a7, 6, 6);
  T2 (a3[1].ma3[2].a7, a3[1].ma3[1].a7, 6, 6);
  T2 (a3[1].ma3[2].a7, a3[1].ma3[2].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }

  T2 (a3[1].ma3[0].a7, a3[2].ma3[0].a7, 6, 6);
  T2 (a3[1].ma3[0].a7, a3[2].ma3[1].a7, 6, 6);
  T2 (a3[1].ma3[0].a7, a3[2].ma3[2].a7, 6, 6);

  T2 (a3[1].ma3[1].a7, a3[2].ma3[0].a7, 6, 6);
  T2 (a3[1].ma3[1].a7, a3[2].ma3[1].a7, 6, 6);
  T2 (a3[1].ma3[1].a7, a3[2].ma3[2].a7, 6, 6);

  T2 (a3[1].ma3[2].a7, a3[2].ma3[0].a7, 6, 6);
  T2 (a3[1].ma3[2].a7, a3[2].ma3[1].a7, 6, 6);
  T2 (a3[1].ma3[2].a7, a3[2].ma3[2].a7, 6, 6);


  T2 (a3[2].ma3[0].a7, a3[0].ma3[0].a7, 6, 6);
  T2 (a3[2].ma3[0].a7, a3[0].ma3[1].a7, 6, 6);
  T2 (a3[2].ma3[0].a7, a3[0].ma3[2].a7, 6, 6);

  T2 (a3[2].ma3[1].a7, a3[0].ma3[0].a7, 6, 6);
  T2 (a3[2].ma3[1].a7, a3[0].ma3[1].a7, 6, 6);
  T2 (a3[2].ma3[1].a7, a3[0].ma3[2].a7, 6, 6);

  T2 (a3[2].ma3[2].a7, a3[0].ma3[0].a7, 6, 6);
  T2 (a3[2].ma3[2].a7, a3[0].ma3[1].a7, 6, 6);
  T2 (a3[2].ma3[2].a7, a3[0].ma3[2].a7, 6, 6);

  T2 (a3[2].ma3[0].a7, a3[1].ma3[0].a7, 6, 6);
  T2 (a3[2].ma3[0].a7, a3[1].ma3[1].a7, 6, 6);
  T2 (a3[2].ma3[0].a7, a3[1].ma3[2].a7, 6, 6);

  T2 (a3[2].ma3[1].a7, a3[1].ma3[0].a7, 6, 6);
  T2 (a3[2].ma3[1].a7, a3[1].ma3[1].a7, 6, 6);
  T2 (a3[2].ma3[1].a7, a3[1].ma3[2].a7, 6, 6);

  T2 (a3[2].ma3[2].a7, a3[1].ma3[0].a7, 6, 6);
  T2 (a3[2].ma3[2].a7, a3[1].ma3[1].a7, 6, 6);
  T2 (a3[2].ma3[2].a7, a3[1].ma3[2].a7, 6, 6);

  T2 (a3[2].ma3[0].a7, a3[2].ma3[0].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
  T2 (a3[2].ma3[0].a7, a3[2].ma3[1].a7, 6, 6);
  T2 (a3[2].ma3[0].a7, a3[2].ma3[2].a7, 6, 6);

  T2 (a3[2].ma3[1].a7, a3[2].ma3[0].a7, 6, 6);
  T2 (a3[2].ma3[1].a7, a3[2].ma3[1].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
  T2 (a3[2].ma3[1].a7, a3[2].ma3[2].a7, 6, 6);

  T2 (a3[2].ma3[2].a7, a3[2].ma3[0].a7, 6, 6);
  T2 (a3[2].ma3[2].a7, a3[2].ma3[1].a7, 6, 6);
  T2 (a3[2].ma3[2].a7, a3[2].ma3[2].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
}


void strcat_arrays_of_arrays_ref (struct ArraysOfMemArrays *p)
{
  T2 (p[0].ma3[0].a7, p[0].ma3[0].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
  T2 (p[0].ma3[0].a7, p[0].ma3[1].a7, 6, 6);
  T2 (p[0].ma3[0].a7, p[0].ma3[2].a7, 6, 6);

  T2 (p[0].ma3[1].a7, p[0].ma3[0].a7, 6, 6);
  T2 (p[0].ma3[1].a7, p[0].ma3[1].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
  T2 (p[0].ma3[1].a7, p[0].ma3[2].a7, 6, 6);

  T2 (p[0].ma3[2].a7, p[0].ma3[0].a7, 6, 6);
  T2 (p[0].ma3[2].a7, p[0].ma3[1].a7, 6, 6);
  T2 (p[0].ma3[2].a7, p[0].ma3[2].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }

  T2 (p[0].ma3[0].a7, p[1].ma3[0].a7, 6, 6);
  T2 (p[0].ma3[0].a7, p[1].ma3[1].a7, 6, 6);
  T2 (p[0].ma3[0].a7, p[1].ma3[2].a7, 6, 6);

  T2 (p[0].ma3[1].a7, p[1].ma3[0].a7, 6, 6);
  T2 (p[0].ma3[1].a7, p[1].ma3[1].a7, 6, 6);
  T2 (p[0].ma3[1].a7, p[1].ma3[2].a7, 6, 6);

  T2 (p[0].ma3[2].a7, p[1].ma3[0].a7, 6, 6);
  T2 (p[0].ma3[2].a7, p[1].ma3[1].a7, 6, 6);
  T2 (p[0].ma3[2].a7, p[1].ma3[2].a7, 6, 6);

  T2 (p[0].ma3[0].a7, p[2].ma3[0].a7, 6, 6);
  T2 (p[0].ma3[0].a7, p[2].ma3[1].a7, 6, 6);
  T2 (p[0].ma3[0].a7, p[2].ma3[2].a7, 6, 6);

  T2 (p[0].ma3[1].a7, p[2].ma3[0].a7, 6, 6);
  T2 (p[0].ma3[1].a7, p[2].ma3[1].a7, 6, 6);
  T2 (p[0].ma3[1].a7, p[2].ma3[2].a7, 6, 6);

  T2 (p[0].ma3[2].a7, p[2].ma3[0].a7, 6, 6);
  T2 (p[0].ma3[2].a7, p[2].ma3[1].a7, 6, 6);
  T2 (p[0].ma3[2].a7, p[2].ma3[2].a7, 6, 6);


  T2 (p[1].ma3[0].a7, p[0].ma3[0].a7, 6, 6);
  T2 (p[1].ma3[0].a7, p[0].ma3[1].a7, 6, 6);
  T2 (p[1].ma3[0].a7, p[0].ma3[2].a7, 6, 6);

  T2 (p[1].ma3[1].a7, p[0].ma3[0].a7, 6, 6);
  T2 (p[1].ma3[1].a7, p[0].ma3[1].a7, 6, 6);
  T2 (p[1].ma3[1].a7, p[0].ma3[2].a7, 6, 6);

  T2 (p[1].ma3[2].a7, p[0].ma3[0].a7, 6, 6);
  T2 (p[1].ma3[2].a7, p[0].ma3[1].a7, 6, 6);
  T2 (p[1].ma3[2].a7, p[0].ma3[2].a7, 6, 6);

  T2 (p[1].ma3[0].a7, p[1].ma3[0].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
  T2 (p[1].ma3[0].a7, p[1].ma3[1].a7, 6, 6);
  T2 (p[1].ma3[0].a7, p[1].ma3[2].a7, 6, 6);

  T2 (p[1].ma3[1].a7, p[1].ma3[0].a7, 6, 6);
  T2 (p[1].ma3[1].a7, p[1].ma3[1].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
  T2 (p[1].ma3[1].a7, p[1].ma3[2].a7, 6, 6);

  T2 (p[1].ma3[2].a7, p[1].ma3[0].a7, 6, 6);
  T2 (p[1].ma3[2].a7, p[1].ma3[1].a7, 6, 6);
  T2 (p[1].ma3[2].a7, p[1].ma3[2].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }

  T2 (p[1].ma3[0].a7, p[2].ma3[0].a7, 6, 6);
  T2 (p[1].ma3[0].a7, p[2].ma3[1].a7, 6, 6);
  T2 (p[1].ma3[0].a7, p[2].ma3[2].a7, 6, 6);

  T2 (p[1].ma3[1].a7, p[2].ma3[0].a7, 6, 6);
  T2 (p[1].ma3[1].a7, p[2].ma3[1].a7, 6, 6);
  T2 (p[1].ma3[1].a7, p[2].ma3[2].a7, 6, 6);

  T2 (p[1].ma3[2].a7, p[2].ma3[0].a7, 6, 6);
  T2 (p[1].ma3[2].a7, p[2].ma3[1].a7, 6, 6);
  T2 (p[1].ma3[2].a7, p[2].ma3[2].a7, 6, 6);


  T2 (p[2].ma3[0].a7, p[0].ma3[0].a7, 6, 6);
  T2 (p[2].ma3[0].a7, p[0].ma3[1].a7, 6, 6);
  T2 (p[2].ma3[0].a7, p[0].ma3[2].a7, 6, 6);

  T2 (p[2].ma3[1].a7, p[0].ma3[0].a7, 6, 6);
  T2 (p[2].ma3[1].a7, p[0].ma3[1].a7, 6, 6);
  T2 (p[2].ma3[1].a7, p[0].ma3[2].a7, 6, 6);

  T2 (p[2].ma3[2].a7, p[0].ma3[0].a7, 6, 6);
  T2 (p[2].ma3[2].a7, p[0].ma3[1].a7, 6, 6);
  T2 (p[2].ma3[2].a7, p[0].ma3[2].a7, 6, 6);

  T2 (p[2].ma3[0].a7, p[1].ma3[0].a7, 6, 6);
  T2 (p[2].ma3[0].a7, p[1].ma3[1].a7, 6, 6);
  T2 (p[2].ma3[0].a7, p[1].ma3[2].a7, 6, 6);

  T2 (p[2].ma3[1].a7, p[1].ma3[0].a7, 6, 6);
  T2 (p[2].ma3[1].a7, p[1].ma3[1].a7, 6, 6);
  T2 (p[2].ma3[1].a7, p[1].ma3[2].a7, 6, 6);

  T2 (p[2].ma3[2].a7, p[1].ma3[0].a7, 6, 6);
  T2 (p[2].ma3[2].a7, p[1].ma3[1].a7, 6, 6);
  T2 (p[2].ma3[2].a7, p[1].ma3[2].a7, 6, 6);

  T2 (p[2].ma3[0].a7, p[2].ma3[0].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
  T2 (p[2].ma3[0].a7, p[2].ma3[1].a7, 6, 6);
  T2 (p[2].ma3[0].a7, p[2].ma3[2].a7, 6, 6);

  T2 (p[2].ma3[1].a7, p[2].ma3[0].a7, 6, 6);
  T2 (p[2].ma3[1].a7, p[2].ma3[1].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
  T2 (p[2].ma3[1].a7, p[2].ma3[2].a7, 6, 6);

  T2 (p[2].ma3[2].a7, p[2].ma3[0].a7, 6, 6);
  T2 (p[2].ma3[2].a7, p[2].ma3[1].a7, 6, 6);
  T2 (p[2].ma3[2].a7, p[2].ma3[2].a7, 6, 6);   // { dg-warning "\\\[-Warray-bounds" }
}
