/* Test BTF generation for enum-type bitfields

   It is allowed to have a bitfield type be an enum type.
   We expect the following types:

     [1] enum 'foo'(1U#B) size=4U#B
	 'BAR' value=0
	 'BAZ' value=1
	 'QUZ' value=2
	 'QUX' value=3
     [2] int 'unsigned int' size=4 offset=0 bits=32
     [3] struct 'bitt' size=4
         member 'f' type=1 bitfield_size=2 bit_offset=0
   */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -gno-strict-dwarf -dA" } */

/* Enum with 4 members.  */
/* { dg-final { scan-assembler-times "\[\t \]0x6000004\[\t \]+\[^\n\]*btt_info" 1 } } */
/* Struct with 1 bitfield member.  */
/* { dg-final { scan-assembler-times "\[\t \]0x84000001\[\t \]+\[^\n\]*btt_info" 1 } } */

/* Bitfield "f" points to type ID 1.  */
/* { dg-final { scan-assembler-times " btm_type: \\(BTF_KIND_ENUM 'foo'" 1 } } */

enum foo
{
  BAR = 0,
  BAZ = 1,
  QUZ = 2,
  QUX = 3
};

struct bitt
{
  enum foo f : 2;
} bitty;
