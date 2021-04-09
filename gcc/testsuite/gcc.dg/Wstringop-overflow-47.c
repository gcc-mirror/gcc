/* Verify that storing a bigger vector into smaller space is diagnosed.
   { dg-do compile }
   { dg-options "-O2" } */

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

/* The tests below fail as a result of the hack for PR 96963.  However,
   with -Wall, the invalid stores are diagnosed by -Warray-bounds which
   runs before vectorization and so doesn't need the hack.  If/when
   -Warray changes to use compute_objsize() this will need adjusting.  */

void warn_c32 (char c)
{
  extern char warn_a32[32];   // { dg-message "at offset 32 into destination object 'warn_a32' of size 32" "pr97027" { xfail *-*-* } }

  void *p = warn_a32 + 1;
  *(C32*)p = (C32){ c };      // { dg-warning "writing 1 byte into a region of size 0" "pr97027" { xfail *-*-* } }

  /* Verify a local variable too. */
  char a32[32];
  p = a32 + 1;
  *(C32*)p = (C32){ c };      // { dg-warning "writing 1 byte into a region of size 0" "pr97027" { xfail *-*-* } }
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
  extern char warn_a64[64];   // { dg-message "at offset 128 to object 'warn_a64' with size 64" "pr97027" { xfail *-*-* } }

  void *p = warn_a64 + 1;
  I16_64 *q = (I16_64*)p;
  *q = (I16_64){ i };         // { dg-warning "writing 1 byte into a region of size 0" "pr97027" { xfail *-*-* } }

  char a64[64];
  p = a64 + 1;
  q = (I16_64*)p;
  *q = (I16_64){ i };         // { dg-warning "writing 1 byte into a region of size 0" "pr97027" { xfail *-*-* } }
  sink (p);
}
