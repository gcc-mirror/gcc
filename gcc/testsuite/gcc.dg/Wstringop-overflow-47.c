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

/* The tests below failed as a result of the hack for PR 96963.  However,
   with -Wall, the invalid stores were diagnosed by -Warray-bounds which
   runs before vectorization and so doesn't need the hack.  Now that
   -Warray-bounds has changed to use compute_objsize() the tests pass.  */

void warn_c32 (char c)
{
  extern char warn_a32[32];   // { dg-message "at offset (32|1) into destination object 'warn_a32' of size 32" "pr97027" }

  void *p = warn_a32 + 1;
  *(C32*)p = (C32){ c };      // { dg-warning "writing (1 byte|32 bytes) into a region of size (0|31)" "pr97027" }

  /* Verify a local variable too. */
  char a32[32];
  p = a32 + 1;
  *(C32*)p = (C32){ c };      // { dg-warning "writing (1 byte|32 bytes) into a region of size (0|31)" "pr97027" }
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
/* The IL below that's visible to the warning changes from one target to
   another.  On some like aarch64 it's a single vector store, on others
   like x86_64 it's a series of BIT_FIELD_REFs.  The overflow by
   the former is detected but the latter is not yet.  */

 extern char warn_a64[64];   // { dg-message "at offset (1|128) into destination object 'warn_a64' of size (63|64)" "pr97027 note" { xfail { ! { aarch64-*-* riscv*-*-* } } } }

  void *p = warn_a64 + 1;
  I16_64 *q = (I16_64*)p;
  *q = (I16_64){ i };         // { dg-warning "writing (1 byte|64 bytes) into a region of size (0|63)" "pr97027" { xfail { ! { aarch64-*-* riscv*-*-* } } } }

  char a64[64];
  p = a64 + 1;
  q = (I16_64*)p;
  *q = (I16_64){ i };         // { dg-warning "writing (1 byte|64 bytes) into a region of size (0|63)" "pr97027" { xfail { ! { aarch64-*-* riscv*-*-* } } } }
  sink (p);
}
