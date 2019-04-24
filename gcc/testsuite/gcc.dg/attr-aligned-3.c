/* PR c/89812 - incorrect maximum in error: requested alignment '536870912'
   exceeds maximum 2147483648
   Limit to ELF targets that are known to use MAX_OFILE_ALIGNMENT
   (1 << 28) * BITS_PER_UNIT.
   { dg-do compile { target { { *-*-elf* *-*-gnu* *-*-solaris2.* } && { ! avr*-*-* } } } }
   { dg-require-effective-target size32plus }
   { dg-options "" } */

#define POWALIGN(N) __attribute__ ((aligned ((__UINT64_TYPE__)1 << (N))))

typedef POWALIGN (28) char T28;

/* The maximum alignment is constrained by the number of bits in int
   on host minus 3: HOST_BITS_PER_INT - LOG2_BITS_PER_UNIT.  The test
   assumes host int is 32-bits wide.  */
typedef POWALIGN (29) char X29;  /* { dg-error "requested alignment .536870912. exceeds maximum 268435456" } */
typedef POWALIGN (30) char X30;  /* { dg-error "requested alignment .1073741824. exceeds maximum 268435456" } */
typedef POWALIGN (31) char X31;  /* { dg-error "requested alignment .2147483648. exceeds maximum 268435456" } */
typedef POWALIGN (32) char X32;  /* { dg-error "requested alignment .4294967296. exceeds maximum 268435456" } */
typedef POWALIGN (60) char X60;  /* { dg-error "requested alignment .1152921504606846976. exceeds maximum 268435456" } */
typedef POWALIGN (63) char X63;  /* { dg-error "requested alignment .9223372036854775808. exceeds maximum 268435456" } */


POWALIGN (28) char c28;

POWALIGN (29) char c29;  /* { dg-error "requested alignment .536870912. exceeds object file maximum 268435456" } */
POWALIGN (30) char x30;  /* { dg-error "requested alignment .1073741824. exceeds object file maximum 268435456" } */
POWALIGN (31) char x31;  /* { dg-error "requested alignment .2147483648. exceeds object file maximum 268435456" } */
POWALIGN (32) char x32;  /* { dg-error "requested alignment .4294967296. exceeds object file maximum 268435456" } */
POWALIGN (60) char x60;  /* { dg-error "requested alignment .1152921504606846976. exceeds object file maximum 268435456" } */
POWALIGN (63) char x63;  /* { dg-error "requested alignment .9223372036854775808. exceeds object file maximum 268435456" } */
