/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-pro_and_epilogue" } */

extern char *asm_out_file;
extern void default_elf_asm_output_ascii (char *, const char *, int);

void
assemble_string (const char *p, int size)
{
  int pos = 0;
  int maximum = 2000;

  while (pos < size)
    {
      int thissize = size - pos;

      if (thissize > maximum)
	thissize = maximum;

      default_elf_asm_output_ascii (asm_out_file, p, thissize);;

      pos += thissize;
      p += thissize;
    }
}

/* { dg-final { scan-rtl-dump "Performing shrink-wrapping" "pro_and_epilogue"  } } */
