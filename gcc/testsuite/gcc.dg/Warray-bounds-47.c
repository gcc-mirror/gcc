/* PR middle-end/91830 - Bogus -Warray-bounds on strcpy into a member
   of a subobject compiling binutils
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

extern char* strcpy (char*, const char*);
extern void sink (void*);

#define S36 "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#define S(N)   (S36 + sizeof (S36) - N - 1)

/* In the test macro, prevent the strcpy to memcpy transformation
   by using a local array initialized with the string literal.  Without
   it, GCC transforms the strcpy call with memcpy which (unfortunately)
   permits accesses that cross subobject boundaries.  */
#define T(obj, mem, n)				\
  do {						\
    struct A *pa = &obj;			\
    const char a[] = S36;			\
    strcpy (pa->mem, a + sizeof a - n - 1);	\
    sink (&obj);				\
  } while (0)


struct A { char a[3]; char b[5]; };
struct B { char c[7]; struct A a; struct A a2[2]; };

extern struct B b[];

void array_nowarn (int i)
{
  struct B *pb = b;

  T (pb[0].a, a, 0);          // { dg-bogus "\\\[-W" }
  T (pb[0].a, a, 1);          // { dg-bogus "\\\[-W" }
  T (pb[0].a, a, 2);          // { dg-bogus "\\\[-W" }

  T (pb[1].a, a, 0);          // { dg-bogus "\\\[-W" }
  T (pb[1].a, a, 1);          // { dg-bogus "\\\[-W" }
  T (pb[1].a, a, 2);          // { dg-bogus "\\\[-W" }

  T (pb[123].a, a, 0);        // { dg-bogus "\\\[-W" }
  T (pb[123].a, a, 1);        // { dg-bogus "\\\[-W" }
  T (pb[123].a, a, 2);        // { dg-bogus "\\\[-W" }

  T (pb[i].a, a, 0);
  T (pb[i].a, a, 1);
  T (pb[i].a, a, 2);


  T (pb[0].a, b, 0);          // { dg-bogus "\\\[-W" }
  T (pb[0].a, b, 1);          // { dg-bogus "\\\[-W" }
  T (pb[0].a, b, 2);          // { dg-bogus "\\\[-W" }
  T (pb[0].a, b, 3);          // { dg-bogus "\\\[-W" }
  T (pb[0].a, b, 4);          // { dg-bogus "\\\[-W" }

  T (pb[1].a, b, 0);          // { dg-bogus "\\\[-W" }
  T (pb[1].a, b, 1);          // { dg-bogus "\\\[-W" }
  T (pb[1].a, b, 2);          // { dg-bogus "\\\[-W" }
  T (pb[1].a, b, 3);          // { dg-bogus "\\\[-W" }
  T (pb[1].a, b, 4);          // { dg-bogus "\\\[-W" }

  T (pb[123].a, b, 0);        // { dg-bogus "\\\[-W" }
  T (pb[123].a, b, 1);        // { dg-bogus "\\\[-W" }
  T (pb[123].a, b, 2);        // { dg-bogus "\\\[-W" }
  T (pb[123].a, b, 3);        // { dg-bogus "\\\[-W" }
  T (pb[123].a, b, 4);        // { dg-bogus "\\\[-W" }

  T (pb[i].a, b, 0);
  T (pb[i].a, b, 1);
  T (pb[i].a, b, 2);
  T (pb[i].a, b, 3);
  T (pb[i].a, b, 4);


  T (pb[0].a2[0], b, 0);      // { dg-bogus "\\\[-W" }
  T (pb[0].a2[0], b, 1);      // { dg-bogus "\\\[-W" }
  T (pb[0].a2[0], b, 2);      // { dg-bogus "\\\[-W" }
  T (pb[0].a2[0], b, 3);      // { dg-bogus "\\\[-W" }
  T (pb[0].a2[0], b, 4);      // { dg-bogus "\\\[-W" }

  T (pb[1].a2[0], b, 0);      // { dg-bogus "\\\[-W" }
  T (pb[1].a2[0], b, 1);      // { dg-bogus "\\\[-W" }
  T (pb[1].a2[0], b, 2);      // { dg-bogus "\\\[-W" }
  T (pb[1].a2[0], b, 3);      // { dg-bogus "\\\[-W" }
  T (pb[1].a2[0], b, 4);      // { dg-bogus "\\\[-W" }

  T (pb[123].a2[0], b, 0);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[0], b, 1);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[0], b, 2);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[0], b, 3);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[0], b, 4);    // { dg-bogus "\\\[-W" }

  T (pb[123].a2[1], b, 0);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[1], b, 1);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[1], b, 2);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[1], b, 3);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[1], b, 4);    // { dg-bogus "\\\[-W" }

  T (pb[i].a2[0], b, 0);
  T (pb[i].a2[0], b, 1);
  T (pb[i].a2[0], b, 2);
  T (pb[i].a2[0], b, 3);
  T (pb[i].a2[0], b, 4);

  T (pb[i].a2[1], b, 0);
  T (pb[i].a2[1], b, 1);
  T (pb[i].a2[1], b, 2);
  T (pb[i].a2[1], b, 3);
  T (pb[i].a2[1], b, 4);
}

void array_warn (int i)
{
  struct B *pb = b;

  T (pb[0].a, a, 3);          // { dg-warning "\\\[-Warray-bounds" }
  T (pb[0].a, a, 4);          // { dg-warning "\\\[-Warray-bounds" }

  T (pb[1].a, a, 5);          // { dg-warning "\\\[-Warray-bounds" }
  T (pb[1].a, a, 6);          // { dg-warning "\\\[-Warray-bounds" }

  T (pb[789].a, a, 5);        // { dg-warning "\\\[-Warray-bounds" }
  T (pb[789].a, a, 6);        // { dg-warning "\\\[-Warray-bounds" }

  T (pb[i].a, a, 7);          // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a, a, 8);          // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }


  T (pb[0].a, b, 5);          // { dg-warning "\\\[-Warray-bounds" }
  T (pb[0].a, b, 6);          // { dg-warning "\\\[-Warray-bounds" }

  T (pb[1].a, b, 5);          // { dg-warning "\\\[-Warray-bounds" }
  T (pb[1].a, b, 6);          // { dg-warning "\\\[-Warray-bounds" }

  T (pb[789].a, b, 5);        // { dg-warning "\\\[-Warray-bounds" }
  T (pb[789].a, b, 6);        // { dg-warning "\\\[-Warray-bounds" }

  T (pb[i].a, b, 5);          // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a, b, 6);          // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }


  T (pb[0].a2[0], b, 5);      // { dg-warning "\\\[-Warray-bounds" }
  T (pb[0].a2[0], b, 6);      // { dg-warning "\\\[-Warray-bounds" }

  T (pb[1].a2[0], b, 5);      // { dg-warning "\\\[-Warray-bounds" }
  T (pb[1].a2[0], b, 6);      // { dg-warning "\\\[-Warray-bounds" }

  T (pb[789].a2[0], b, 5);    // { dg-warning "\\\[-Warray-bounds" }
  T (pb[789].a2[0], b, 6);    // { dg-warning "\\\[-Warray-bounds" }

  T (pb[i].a2[0], b, 5);      // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a2[0], b, 6);      // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }

  T (pb[0].a2[1], b, 5);      // { dg-warning "\\\[-Warray-bounds" }
  T (pb[0].a2[1], b, 6);      // { dg-warning "\\\[-Warray-bounds" }

  T (pb[1].a2[1], b, 5);      // { dg-warning "\\\[-Warray-bounds" }
  T (pb[1].a2[1], b, 6);      // { dg-warning "\\\[-Warray-bounds" }

  T (pb[789].a2[1], b, 5);    // { dg-warning "\\\[-Warray-bounds" }
  T (pb[789].a2[1], b, 6);    // { dg-warning "\\\[-Warray-bounds" }

  T (pb[i].a2[1], b, 5);      // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a2[1], b, 6);      // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
}

void ptr_nowarn (struct B *pb, int i)
{
  T (pb[-123].a, a, 0);       // { dg-bogus "\\\[-W" }
  T (pb[-123].a, a, 1);       // { dg-bogus "\\\[-W" }
  T (pb[-123].a, a, 2);       // { dg-bogus "\\\[-W" }

  T (pb[-2].a, a, 0);         // { dg-bogus "\\\[-W" }
  T (pb[-2].a, a, 1);         // { dg-bogus "\\\[-W" }
  T (pb[-2].a, a, 2);         // { dg-bogus "\\\[-W" }

  T (pb[-1].a, a, 0);         // { dg-bogus "\\\[-W" }
  T (pb[-1].a, a, 1);         // { dg-bogus "\\\[-W" }
  T (pb[-1].a, a, 2);         // { dg-bogus "\\\[-W" }

  T (pb[0].a, a, 0);          // { dg-bogus "\\\[-W" }
  T (pb[0].a, a, 1);          // { dg-bogus "\\\[-W" }
  T (pb[0].a, a, 2);          // { dg-bogus "\\\[-W" }

  T (pb[1].a, a, 0);          // { dg-bogus "\\\[-W" }
  T (pb[1].a, a, 1);          // { dg-bogus "\\\[-W" }
  T (pb[1].a, a, 2);          // { dg-bogus "\\\[-W" }

  T (pb[123].a, a, 0);        // { dg-bogus "\\\[-W" }
  T (pb[123].a, a, 1);        // { dg-bogus "\\\[-W" }
  T (pb[123].a, a, 2);        // { dg-bogus "\\\[-W" }

  T (pb[i].a, a, 0);          // { dg-bogus "\\\[-W" }
  T (pb[i].a, a, 1);          // { dg-bogus "\\\[-W" }
  T (pb[i].a, a, 2);          // { dg-bogus "\\\[-W" }


  T (pb[-123].a, b, 0);       // { dg-bogus "\\\[-W" }
  T (pb[-123].a, b, 1);       // { dg-bogus "\\\[-W" }
  T (pb[-123].a, b, 2);       // { dg-bogus "\\\[-W" }
  T (pb[-123].a, b, 3);       // { dg-bogus "\\\[-W" }
  T (pb[-123].a, b, 4);       // { dg-bogus "\\\[-W" }

  T (pb[-2].a, b, 0);         // { dg-bogus "\\\[-W" }
  T (pb[-2].a, b, 1);         // { dg-bogus "\\\[-W" }
  T (pb[-2].a, b, 2);         // { dg-bogus "\\\[-W" }
  T (pb[-2].a, b, 3);         // { dg-bogus "\\\[-W" }
  T (pb[-2].a, b, 4);         // { dg-bogus "\\\[-W" }

  T (pb[-1].a, b, 0);         // { dg-bogus "\\\[-W" }
  T (pb[-1].a, b, 1);         // { dg-bogus "\\\[-W" }
  T (pb[-1].a, b, 2);         // { dg-bogus "\\\[-W" }
  T (pb[-1].a, b, 3);         // { dg-bogus "\\\[-W" }
  T (pb[-1].a, b, 4);         // { dg-bogus "\\\[-W" }

  T (pb[0].a, b, 0);          // { dg-bogus "\\\[-W" }
  T (pb[0].a, b, 1);          // { dg-bogus "\\\[-W" }
  T (pb[0].a, b, 2);          // { dg-bogus "\\\[-W" }
  T (pb[0].a, b, 3);          // { dg-bogus "\\\[-W" }
  T (pb[0].a, b, 4);          // { dg-bogus "\\\[-W" }

  T (pb[1].a, b, 0);          // { dg-bogus "\\\[-W" }
  T (pb[1].a, b, 1);          // { dg-bogus "\\\[-W" }
  T (pb[1].a, b, 2);          // { dg-bogus "\\\[-W" }
  T (pb[1].a, b, 3);          // { dg-bogus "\\\[-W" }
  T (pb[1].a, b, 4);          // { dg-bogus "\\\[-W" }

  T (pb[123].a, b, 0);        // { dg-bogus "\\\[-W" }
  T (pb[123].a, b, 1);        // { dg-bogus "\\\[-W" }
  T (pb[123].a, b, 2);        // { dg-bogus "\\\[-W" }
  T (pb[123].a, b, 3);        // { dg-bogus "\\\[-W" }
  T (pb[123].a, b, 4);        // { dg-bogus "\\\[-W" }

  T (pb[i].a, b, 0);
  T (pb[i].a, b, 1);
  T (pb[i].a, b, 2);
  T (pb[i].a, b, 3);
  T (pb[i].a, b, 4);


  T (pb[-123].a2[0], b, 0);   // { dg-bogus "\\\[-W" }
  T (pb[-123].a2[0], b, 1);   // { dg-bogus "\\\[-W" }
  T (pb[-123].a2[0], b, 2);   // { dg-bogus "\\\[-W" }
  T (pb[-123].a2[0], b, 3);   // { dg-bogus "\\\[-W" }
  T (pb[-123].a2[0], b, 4);   // { dg-bogus "\\\[-W" }

  T (pb[-2].a2[0], b, 0);     // { dg-bogus "\\\[-W" }
  T (pb[-2].a2[0], b, 1);     // { dg-bogus "\\\[-W" }
  T (pb[-2].a2[0], b, 2);     // { dg-bogus "\\\[-W" }
  T (pb[-2].a2[0], b, 3);     // { dg-bogus "\\\[-W" }
  T (pb[-2].a2[0], b, 4);     // { dg-bogus "\\\[-W" }

  T (pb[-1].a2[0], b, 0);     // { dg-bogus "\\\[-W" }
  T (pb[-1].a2[0], b, 1);     // { dg-bogus "\\\[-W" }
  T (pb[-1].a2[0], b, 2);     // { dg-bogus "\\\[-W" }
  T (pb[-1].a2[0], b, 3);     // { dg-bogus "\\\[-W" }
  T (pb[-1].a2[0], b, 4);     // { dg-bogus "\\\[-W" }

  T (pb[0].a2[0], b, 0);      // { dg-bogus "\\\[-W" }
  T (pb[0].a2[0], b, 1);      // { dg-bogus "\\\[-W" }
  T (pb[0].a2[0], b, 2);      // { dg-bogus "\\\[-W" }
  T (pb[0].a2[0], b, 3);      // { dg-bogus "\\\[-W" }
  T (pb[0].a2[0], b, 4);      // { dg-bogus "\\\[-W" }

  T (pb[1].a2[0], b, 0);      // { dg-bogus "\\\[-W" }
  T (pb[1].a2[0], b, 1);      // { dg-bogus "\\\[-W" }
  T (pb[1].a2[0], b, 2);      // { dg-bogus "\\\[-W" }
  T (pb[1].a2[0], b, 3);      // { dg-bogus "\\\[-W" }
  T (pb[1].a2[0], b, 4);      // { dg-bogus "\\\[-W" }

  T (pb[123].a2[0], b, 0);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[0], b, 1);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[0], b, 2);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[0], b, 3);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[0], b, 4);    // { dg-bogus "\\\[-W" }

  T (pb[i].a2[0], b, 0);
  T (pb[i].a2[0], b, 1);
  T (pb[i].a2[0], b, 2);
  T (pb[i].a2[0], b, 3);
  T (pb[i].a2[0], b, 4);

  T (pb[-123].a2[1], b, 0);   // { dg-bogus "\\\[-W" }
  T (pb[-123].a2[1], b, 1);   // { dg-bogus "\\\[-W" }
  T (pb[-123].a2[1], b, 2);   // { dg-bogus "\\\[-W" }
  T (pb[-123].a2[1], b, 3);   // { dg-bogus "\\\[-W" }
  T (pb[-123].a2[1], b, 4);   // { dg-bogus "\\\[-W" }

  T (pb[-2].a2[1], b, 0);     // { dg-bogus "\\\[-W" }
  T (pb[-2].a2[1], b, 1);     // { dg-bogus "\\\[-W" }
  T (pb[-2].a2[1], b, 2);     // { dg-bogus "\\\[-W" }
  T (pb[-2].a2[1], b, 3);     // { dg-bogus "\\\[-W" }
  T (pb[-2].a2[1], b, 4);     // { dg-bogus "\\\[-W" }

  T (pb[-1].a2[1], b, 0);     // { dg-bogus "\\\[-W" }
  T (pb[-1].a2[1], b, 1);     // { dg-bogus "\\\[-W" }
  T (pb[-1].a2[1], b, 2);     // { dg-bogus "\\\[-W" }
  T (pb[-1].a2[1], b, 3);     // { dg-bogus "\\\[-W" }
  T (pb[-1].a2[1], b, 4);     // { dg-bogus "\\\[-W" }

  T (pb[0].a2[1], b, 0);      // { dg-bogus "\\\[-W" }
  T (pb[0].a2[1], b, 1);      // { dg-bogus "\\\[-W" }
  T (pb[0].a2[1], b, 2);      // { dg-bogus "\\\[-W" }
  T (pb[0].a2[1], b, 3);      // { dg-bogus "\\\[-W" }
  T (pb[0].a2[1], b, 4);      // { dg-bogus "\\\[-W" }

  T (pb[1].a2[1], b, 0);      // { dg-bogus "\\\[-W" }
  T (pb[1].a2[1], b, 1);      // { dg-bogus "\\\[-W" }
  T (pb[1].a2[1], b, 2);      // { dg-bogus "\\\[-W" }
  T (pb[1].a2[1], b, 3);      // { dg-bogus "\\\[-W" }
  T (pb[1].a2[1], b, 4);      // { dg-bogus "\\\[-W" }

  T (pb[123].a2[1], b, 0);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[1], b, 1);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[1], b, 2);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[1], b, 3);    // { dg-bogus "\\\[-W" }
  T (pb[123].a2[1], b, 4);    // { dg-bogus "\\\[-W" }

  T (pb[i].a2[1], b, 0);
  T (pb[i].a2[1], b, 1);
  T (pb[i].a2[1], b, 2);
  T (pb[i].a2[1], b, 3);
  T (pb[i].a2[1], b, 4);

  T (pb[i].a2[i], b, 0);
  T (pb[i].a2[i], b, 1);
  T (pb[i].a2[i], b, 2);
  T (pb[i].a2[i], b, 3);
  T (pb[i].a2[i], b, 4);
}

void ptr_warn (struct B *pb, int i)
{
  T (pb[-987].a, a, 8);       // { dg-warning "\\\[-Warray-bounds" }
  T (pb[-654].a, a, 7);       // { dg-warning "\\\[-Warray-bounds" }

  T (pb[-2].a, a, 6);         // { dg-warning "\\\[-Warray-bounds" }
  T (pb[-2].a, a, 5);         // { dg-warning "\\\[-Warray-bounds" }

  T (pb[-1].a, a, 3);         // { dg-warning "\\\[-Warray-bounds" }
  T (pb[-1].a, a, 4);         // { dg-warning "\\\[-Warray-bounds" }

  T (pb[0].a, a, 3);          // { dg-warning "\\\[-Warray-bounds" }
  T (pb[0].a, a, 4);          // { dg-warning "\\\[-Warray-bounds" }

  T (pb[1].a, a, 5);          // { dg-warning "\\\[-Warray-bounds" }
  T (pb[1].a, a, 6);          // { dg-warning "\\\[-Warray-bounds" }

  T (pb[789].a, a, 7);        // { dg-warning "\\\[-Warray-bounds" }
  T (pb[789].a, a, 8);        // { dg-warning "\\\[-Warray-bounds" }

  T (pb[i].a, a, 3);          // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a, a, 4);          // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a, a, 5);          // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }


  T (pb[-987].a, b, 10);      // { dg-warning "\\\[-Warray-bounds" }
  T (pb[-654].a, b, 9);       // { dg-warning "\\\[-Warray-bounds" }

  T (pb[-2].a, b, 8);         // { dg-warning "\\\[-Warray-bounds" }
  T (pb[-2].a, b, 7);         // { dg-warning "\\\[-Warray-bounds" }

  T (pb[-1].a, b, 6);         // { dg-warning "\\\[-Warray-bounds" }
  T (pb[-1].a, b, 5);         // { dg-warning "\\\[-Warray-bounds" }

  T (pb[0].a, b, 5);          // { dg-warning "\\\[-Warray-bounds" }
  T (pb[0].a, b, 6);          // { dg-warning "\\\[-Warray-bounds" }

  T (pb[1].a, b, 7);          // { dg-warning "\\\[-Warray-bounds" }
  T (pb[1].a, b, 8);          // { dg-warning "\\\[-Warray-bounds" }

  T (pb[789].a, b, 9);        // { dg-warning "\\\[-Warray-bounds" }
  T (pb[789].a, b, 10);       // { dg-warning "\\\[-Warray-bounds" }

  T (pb[i].a, b, 5);          // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a, b, 6);          // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a, b, 7);          // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }


  T (pb[-987].a2[0], b, 10);  // { dg-warning "\\\[-Warray-bounds" }
  T (pb[-654].a2[0], b, 9);   // { dg-warning "\\\[-Warray-bounds" }

  T (pb[-2].a2[0], b, 8);     // { dg-warning "\\\[-Warray-bounds" }
  T (pb[-2].a2[0], b, 7);     // { dg-warning "\\\[-Warray-bounds" }

  T (pb[-1].a2[0], b, 6);     // { dg-warning "\\\[-Warray-bounds" }
  T (pb[-1].a2[0], b, 5);     // { dg-warning "\\\[-Warray-bounds" }

  T (pb[0].a2[0], b, 5);      // { dg-warning "\\\[-Warray-bounds" }
  T (pb[0].a2[0], b, 6);      // { dg-warning "\\\[-Warray-bounds" }

  T (pb[1].a2[0], b, 7);      // { dg-warning "\\\[-Warray-bounds" }
  T (pb[1].a2[0], b, 8);      // { dg-warning "\\\[-Warray-bounds" }

  T (pb[789].a2[0], b, 9);    // { dg-warning "\\\[-Warray-bounds" }
  T (pb[789].a2[0], b, 10);   // { dg-warning "\\\[-Warray-bounds" }

  T (pb[i].a2[0], b, 5);      // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a2[0], b, 6);      // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a2[0], b, 7);      // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }

  T (pb[-987].a2[1], b, 10);  // { dg-warning "\\\[-Warray-bounds" }
  T (pb[-654].a2[1], b, 9);   // { dg-warning "\\\[-Warray-bounds" }

  T (pb[-2].a2[1], b, 8);     // { dg-warning "\\\[-Warray-bounds" }
  T (pb[-2].a2[1], b, 7);     // { dg-warning "\\\[-Warray-bounds" }

  T (pb[-1].a2[1], b, 6);     // { dg-warning "\\\[-Warray-bounds" }
  T (pb[-1].a2[1], b, 5);     // { dg-warning "\\\[-Warray-bounds" }

  T (pb[0].a2[1], b, 5);      // { dg-warning "\\\[-Warray-bounds" }
  T (pb[0].a2[1], b, 6);      // { dg-warning "\\\[-Warray-bounds" }

  T (pb[1].a2[1], b, 7);      // { dg-warning "\\\[-Warray-bounds" }
  T (pb[1].a2[1], b, 8);      // { dg-warning "\\\[-Warray-bounds" }

  T (pb[789].a2[1], b, 9);    // { dg-warning "\\\[-Warray-bounds" }
  T (pb[789].a2[1], b, 10);   // { dg-warning "\\\[-Warray-bounds" }

  T (pb[i].a2[1], b, 5);      // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a2[1], b, 6);      // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a2[1], b, 7);      // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }

  T (pb[i].a2[i], b, 5);      // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a2[i], b, 6);      // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
  T (pb[i].a2[i], b, 7);      // { dg-warning "\\\[-Warray-bounds" "pr91848" { xfail *-*-* } }
}
