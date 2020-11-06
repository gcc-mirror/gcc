/* { dg-do compile } */

void
foo ()
{
  int i, v;
#pragma acc atomic read bar  /* { dg-error "expected 'read', 'write', 'update', or 'capture' clause" } */
  i = v;  /* { dg-error "expected end of line before 'bar'" "" { target *-*-* } .-1 } */

#pragma acc atomic read write  /* { dg-error "too many atomic clauses" } */
  i = v;

#pragma acc atomic read seq_cst  /* { dg-error "expected 'read', 'write', 'update', or 'capture' clause" } */
  i = v;  /* { dg-error "expected end of line before 'seq_cst'" "" { target *-*-* } .-1 } */

#pragma acc atomic read relaxed  /* { dg-error "expected 'read', 'write', 'update', or 'capture' clause" } */
  i = v;  /* { dg-error "expected end of line before 'relaxed'" "" { target *-*-* } .-1 } */

#pragma acc atomic update hint(1)  /* { dg-error "expected 'read', 'write', 'update', or 'capture' clause" } */
  i += 1;  /* { dg-error "expected end of line before 'hint'" "" { target *-*-* } .-1 } */

#pragma acc atomic update update capture  /* { dg-error "too many atomic clauses" } */
  v = i += 1;

#pragma acc atomic update capture capture  /* { dg-error "too many atomic clauses" } */
  v = i += 1;

#pragma acc atomic write capture  /* { dg-error "too many atomic clauses" } */
  i = 1;
}
