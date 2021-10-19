/* PR tree-optimization/42587 */
/* { dg-do compile } */
/* { dg-require-effective-target bswap } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-store-merging" } */
/* { dg-additional-options "-march=z900" { target s390-*-* } } */

typedef unsigned char u8;
typedef unsigned int u32;
union __anonunion_out_195
{
  u32 value;
  u8 bytes[4];
};
union __anonunion_in_196
{
  u32 value;
  u8 bytes[4];
};
extern void acpi_ut_track_stack_ptr (void);
u32 acpi_ut_dword_byte_swap (u32 value);
u32
acpi_ut_dword_byte_swap (u32 value)
{
  union __anonunion_out_195 out;
  union __anonunion_in_196 in;

  {
    acpi_ut_track_stack_ptr ();
    in.value = value;
    out.bytes[0] = in.bytes[3];
    out.bytes[1] = in.bytes[2];
    out.bytes[2] = in.bytes[1];
    out.bytes[3] = in.bytes[0];
    return (out.value);
  }
}

/* { dg-final { scan-tree-dump "32 bit bswap implementation found at" "store-merging" } } */
