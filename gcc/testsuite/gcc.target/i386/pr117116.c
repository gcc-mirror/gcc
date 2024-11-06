/* PR target/117116 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */

typedef void (*StmFct)();
typedef struct {
  StmFct fct_getc;
  StmFct fct_putc;
  StmFct fct_flush;
  StmFct fct_close;
} StmInf;

StmInf TTY_Getc_pstm;

void TTY_Getc() {
  TTY_Getc_pstm.fct_getc = TTY_Getc;
  TTY_Getc_pstm.fct_putc = TTY_Getc_pstm.fct_flush = TTY_Getc_pstm.fct_close = (StmFct)1;
}
