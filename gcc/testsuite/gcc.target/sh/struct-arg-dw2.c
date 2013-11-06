/* Verify that we don't generate frame related insn against stack adjustment
   for the object sent partially in registers. */
/* { dg-do compile }  */
/* { dg-options "-g" } */
/* { dg-final { scan-assembler-not "\t.cfi_def_cfa_offset 16" } } */

typedef struct
{
  unsigned short A1;
  unsigned short A2;
} A_t;

typedef struct
{
  A_t C13[10];
} C_t;

void
Store (C_t Par)
{
  unsigned char *ptr;
  unsigned int test;

  ptr = (unsigned char*) 0x12345678;
  ptr++;
}
