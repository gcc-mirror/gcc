/* Test invalid uses of declare directive.  */
/* { dg-do compile } */

#pragma acc declare /* { dg-error "no valid clauses" } */

#pragma acc declare create(undeclared) /* { dg-error "undeclared" } */
/* { dg-error "no valid clauses" "second error" { target *-*-* } .-1 } */

int v0[10];
#pragma acc declare create(v0[1:3]) /* { dg-error "array section" } */

int v1;
#pragma acc declare create(v1, v1) /* { dg-error "more than once" } */

int v2;
#pragma acc declare create(v2)
#pragma acc declare copyin(v2) /* { dg-error "more than once" } */

int v3;
#pragma acc declare copy(v3) /* { dg-error "at file scope" } */

int v4;
#pragma acc declare copyout(v4) /* { dg-error "at file scope" } */

int v5;
#pragma acc declare present(v5) /* { dg-error "at file scope" } */

int v6;
#pragma acc declare present_or_copy(v6) /* { dg-error "at file scope" } */

int v7;
#pragma acc declare present_or_copyin(v7) /* { dg-error "at file scope" } */

int v8;
#pragma acc declare present_or_copyout(v8) /* { dg-error "at file scope" } */

int v9;
#pragma acc declare present_or_create(v9) /* { dg-error "at file scope" } */

int va10;
#pragma acc declare create (va10)
#pragma acc declare link (va10) /* { dg-error "more than once" } */

int va11;
#pragma acc declare link (va11)
#pragma acc declare link (va11) /* { dg-error "more than once" } */

int va12;
#pragma acc declare create (va12) link (va12) /* { dg-error "more than once" } */

void
f (void)
{
  int va0;
#pragma acc declare link(va0) /* { dg-error "global variable" } */

  extern int ve0;
#pragma acc declare copy(ve0) /* { dg-error "invalid use of" } */

  extern int ve1;
#pragma acc declare copyout(ve1) /* { dg-error "invalid use of" } */

  extern int ve2;
#pragma acc declare present(ve2) /* { dg-error "invalid use of" } */

  extern int ve3;
#pragma acc declare present_or_copy(ve3) /* { dg-error "invalid use of" } */

  extern int ve4;
#pragma acc declare present_or_copyin(ve4) /* { dg-error "invalid use of" } */

  extern int ve5;
#pragma acc declare present_or_copyout(ve5) /* { dg-error "invalid use of" } */

  extern int ve6;
#pragma acc declare present_or_create(ve6) /* { dg-error "invalid use of" } */

#pragma acc declare present (v9) /* { dg-error "invalid use of" } */
}
