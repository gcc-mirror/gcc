/* PR/81900 */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-options "-fdump-tree-optimized" } */

void Perl_croak() __attribute__((noreturn));
char *Perl_sv_gets();
void __attribute__((returns_twice)) __sigsetjmp ();
void a() {
    __sigsetjmp();
    char *b;
    if ((b = Perl_sv_gets()) == 0)
      Perl_croak("No Perl script found in input\n");
    if (*b == '#')
      __asm__("" : : ""("badbad"));
}

/* Do not elide the test against zero.  */
/* { dg-final { scan-tree-dump " == 0B" "optimized" } } */
