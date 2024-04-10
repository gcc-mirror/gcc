#include <stdint.h>

/* Attacker-controlled 8 bit values where the array isn't
   necessarily big enough.  We should warn about these.  */

struct st_s8_field_255_elements
{
  int8_t idx;
  char buf[255];
};

char __attribute__((tainted_args))
test_s8_field_255_elements (struct st_s8_field_255_elements s)
{
  return s.buf[s.idx]; /* { dg-warning "tainted-array-index" } */
}

struct st_u8_field_255_elements
{
  uint8_t idx;
  char buf[255];
};

char __attribute__((tainted_args))
test_u8_field_255_elements (struct st_u8_field_255_elements s)
{
  return s.buf[s.idx]; /* { dg-warning "tainted-array-index" } */
}

/* Attacker-controlled 8 bit values where the array is
   big enough, but where the value might be signed.  */

struct st_s8_field_256_elements
{
  int8_t idx;
  char buf[256];
};

char __attribute__((tainted_args))
test_s8_field_256_elements (struct st_s8_field_256_elements s)
{
  return s.buf[s.idx]; /* { dg-warning "tainted-array-index" } */
}

struct st_u8_field_256_elements
{
  uint8_t idx;
  char buf[256];
};

char __attribute__((tainted_args))
test_u8_field_256_elements (struct st_u8_field_256_elements s)
{
  return s.buf[s.idx]; /* { dg-bogus "tainted-array-index" } */
}

/* Attacker-controlled 16 bit values where the array isn't
   necessarily big enough.  We should warn about these.  */

struct st_s16_field_256_elements
{
  int16_t idx;
  char buf[256];
};

char __attribute__((tainted_args))
test_s16_field_256_elements (struct st_s16_field_256_elements s)
{
  return s.buf[s.idx]; /* { dg-warning "tainted-array-index" } */
}

struct st_u16_field_256_elements
{
  uint16_t idx;
  char buf[256];
};

char __attribute__((tainted_args))
test_u16_field_256_elements (struct st_u16_field_256_elements s)
{
  return s.buf[s.idx]; /* { dg-warning "tainted-array-index" } */
}

/* Attacker-controlled 16 bit values where the array is
   big enough, but where the value might be signed.  */

struct st_s16_field_65536_elements
{
  int16_t idx;
  char buf[65536];
};

char __attribute__((tainted_args))
test_s16_field_65536_elements (struct st_s16_field_65536_elements s)
{
  return s.buf[s.idx]; /* { dg-warning "tainted-array-index" } */
}

struct st_u16_field_65536_elements
{
  uint16_t idx;
  char buf[65536];
};

char __attribute__((tainted_args))
test_u16_field_65536_elements (struct st_u16_field_65536_elements s)
{
  return s.buf[s.idx]; /* { dg-bogus "tainted-array-index" } */
}
