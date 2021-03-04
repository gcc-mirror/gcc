/* Verify that storing a bigger vector into smaller space is diagnosed.
   { dg-do compile }
   { dg-options "-O2 -Warray-bounds" } */

typedef __INT16_TYPE__                         int16_t;
typedef __attribute__ ((__vector_size__ (32))) char C32;

typedef __attribute__ ((__vector_size__ (64))) int16_t I16_64;

void sink (void*);


void nowarn_c32 (char c)
{
  extern char nowarn_a32[32];

  void *p = nowarn_a32;
  *(C32*)p = (C32){ c };
  sink (p);

  char a32[32];
  p = a32;
  *(C32*)p = (C32){ c };
  sink (p);
}

/* The invalid stores below are diagnosed by -Warray-bounds only
   because it doesn't use compute_objsize().  If/when that changes
   the function might need adjusting to avoid the hack put in place
   to avoid false positives due to vectorization.  */

void warn_c32 (char c)
{
  extern char warn_a32[32];   // { dg-message "'warn_a32'" "note" }

  void *p = warn_a32 + 1;
  *(C32*)p = (C32){ c };      // { dg-warning "\\\[-Warray-bounds" }

  /* Verify a local variable too. */
  char a32[32];               // { dg-message "'a32'" }
  p = a32 + 1;
  *(C32*)p = (C32){ c };      // { dg-warning "\\\[-Warray-bounds" }
  sink (p);
}


void nowarn_i16_64 (int16_t i)
{
  extern char nowarn_a64[64];

  void *p = nowarn_a64;
  I16_64 *q = (I16_64*)p;
  *q = (I16_64){ i };

  char a64[64];
  q = (I16_64*)a64;
  *q = (I16_64){ i };
  sink (q);
}

void warn_i16_64 (int16_t i)
{
  extern char warn_a64[64];   // { dg-message "'warn_a64'" }

  void *p = warn_a64 + 1;
  I16_64 *q = (I16_64*)p;
  *q = (I16_64){ i };         // { dg-warning "\\\[-Warray-bounds" }

  char a64[64];               // { dg-message "'a64'" }
  p = a64 + 1;
  q = (I16_64*)p;
  *q = (I16_64){ i };         // { dg-warning "\\\[-Warray-bounds" }
  sink (p);
}
