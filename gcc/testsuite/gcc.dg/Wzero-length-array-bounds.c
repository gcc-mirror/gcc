/* PR middle-end/91647 - missing -Warray-bounds accessing a zero-length array
   of a declared object
   Test to exercise -Wzero-length-bounds.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

void sink (void*);

struct X { int a[0]; int b, c; };

extern struct X x;

void bad (int i, int j)
{
  x.a[0] = 0;           // { dg-warning "\\\[-Wzero-length-bounds" }
  x.a[1] = 1;           // { dg-warning "\\\[-Wzero-length-bounds" }
  x.a[2] = 2;           // { dg-warning "\\\[-Warray-bounds" }

  x.a[i] = 3;           // { dg-warning "\\\[-Wzero-length-bounds" }
  x.a[j] = 4;           // { dg-warning "array subscript 'j' is outside the bounds of an interior zero-length array" }
}

void access_by_reference (struct X *p, int i)
{
  p->a[0] = 0;          // { dg-warning "\\\[-Wzero-length-bounds" }
  p->a[1] = 0;          // { dg-warning "\\\[-Wzero-length-bounds" }
  p->a[2] = 0;          // { dg-warning "\\\[-Wzero-length-bounds" }
  p->a[i] = 0;          // { dg-warning "\\\[-Wzero-length-bounds" }
}


extern struct X a[2];

void access_to_array (int i)
{
  a[0].a[0] = 0;        // { dg-warning "\\\[-Wzero-length-bounds" }
  a[0].a[1] = 1;        // { dg-warning "\\\[-Wzero-length-bounds" }
  /* Accesses to a subsequent element of the enclosing array seem like
     a more sever problem than those to the next member of the same
     struct and so might perhaps be better diagnosed by -Warray-bounds.
     Then again, code that does this sort of crap might as well get what
     it deserves if it disables -Wzero-length-bounds.  */
  a[0].a[2] = 2;        // { dg-warning "\\\[-Wzero-length-bounds" }

  a[0].a[i] = 3;        // { dg-warning "\\\[-Wzero-length-bounds" }
  sink (a);

  a[1].a[0] = 4;        // { dg-warning "\\\[-Wzero-length-bounds" }
  a[1].a[1] = 5;        // { dg-warning "\\\[-Wzero-length-bounds" }
  a[1].a[2] = 6;        // { dg-warning "\\\[-Warray-bounds" }

  a[1].a[i] = 7;        // { dg-warning "\\\[-Wzero-length-bounds" }
  sink (a);

  a[i].a[0] = 8;        // { dg-warning "\\\[-Wzero-length-bounds" }
  a[i].a[1] = 9;        // { dg-warning "\\\[-Wzero-length-bounds" }
  a[i].a[2] = 0;        // { dg-warning "\\\[-Wzero-length-bounds" }
}


struct Y
{
  struct X a[2], b;
  int c;
};

extern struct Y y;

void access_to_member (int i)
{
  y.a[0].a[0] = 0;      // { dg-warning "\\\[-Wzero-length-bounds" }
  y.a[0].a[1] = 0;      // { dg-warning "\\\[-Wzero-length-bounds" }
  y.a[0].a[2] = 0;      // { dg-warning "\\\[-Wzero-length-bounds" }
  sink (a);

  y.a[1].a[0] = 0;      // { dg-warning "\\\[-Wzero-length-bounds" }
  y.a[1].a[1] = 0;      // { dg-warning "\\\[-Wzero-length-bounds" }
  /* Similar to the array case above, accesses to a subsequent member
     of the "parent" struct seem like a more severe problem than those
     to the next member of the same struct.  */
  y.a[1].a[2] = 0;      // { dg-warning "\\\[-Wzero-length-bounds" }
  sink (a);

  y.b.a[0] = 0;         // { dg-warning "\\\[-Wzero-length-bounds" }
  y.b.a[1] = 0;         // { dg-warning "\\\[-Wzero-length-bounds" }
  y.b.a[2] = 0;         // { dg-warning "\\\[-Wzero-length-bounds" }
  y.b.a[3] = 0;         // { dg-warning "\\\[-Warray-bounds" }
}
