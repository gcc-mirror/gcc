/* Test that -mrecord-mcount aligns the __mcount_loc entries.  */
/* { dg-do compile { target { *-*-linux* && nonpic } } } */
/* { dg-options "-pg -mrecord-mcount" } */
/* { dg-final { scan-assembler {__mcount_loc, "a",@progbits\n\t\.b?align 8\n\t\.quad 1b} { target { ! ia32 } } } } */
/* { dg-final { scan-assembler {__mcount_loc, "a",@progbits\n\t\.b?align 4\n\t\.long 1b} { target ia32 } } } */

void
func (void)
{
}
