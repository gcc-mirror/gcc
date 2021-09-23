/* Test for compilation of self-referntial structs.

   Further, the compiler is expected to generate a single CTF struct type for
   struct dmx_dtdef (due to Type de-duplication at CTF generation).  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */
/* { dg-final { scan-assembler-times "ascii \"dmx_dtdef.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"dtd_name.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"dtd_type.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"dmx_dtdef_t.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

struct link
{
  struct link * next;
} * s_link;

typedef long dmx_id_t;

typedef struct dmx_dtdef
{
  char * dtd_name;
  dmx_id_t dtd_type;
} dmx_dtdef_t;

typedef struct dmx_bundle
{
  dmx_id_t dmb_type;
  dmx_dtdef_t * dmb_dtd;
} dmx_bundle_t;

dmx_bundle_t dbt;
