/* Test OpenACC's support for manual deep copy, including the attach
   and detach clauses.  */

void
t1 ()
{
  struct foo {
    int *a, *b, c, d, *e;
  } s;

  int *a, *z, scalar, **y;

#pragma acc enter data copyin(s) detach(z) /* { dg-error ".detach. is not valid for" } */
  {
#pragma acc data copy(s.a[0:10]) copy(z[0:10])
    {
      s.e = z;
#pragma acc parallel loop attach(s.e) detach(s.b) /* { dg-error ".detach. is not valid for" } */
      for (int i = 0; i < 10; i++)
        s.a[i] = s.e[i];

      a = s.e;
#pragma acc enter data attach(a) detach(s.c) /* { dg-error ".detach. is not valid for" } */
#pragma acc exit data detach(a)
    }

#pragma acc enter data attach(z[:5]) /* { dg-error "expected single pointer in .attach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc exit data detach(z[:5]) /* { dg-error "expected single pointer in .detach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc enter data attach(z[1:]) /* { dg-error "expected single pointer in .attach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc exit data detach(z[1:]) /* { dg-error "expected single pointer in .detach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc enter data attach(z[:]) /* { dg-error "expected single pointer in .attach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc exit data detach(z[:]) /* { dg-error "expected single pointer in .detach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc enter data attach(z[3]) /* { dg-error "expected pointer in .attach. clause" } */
#pragma acc exit data detach(z[3]) /* { dg-error "expected pointer in .detach. clause" } */

#pragma acc acc enter data attach(s.e)
#pragma acc exit data detach(s.e) attach(z) /* { dg-error ".attach. is not valid for" } */

#pragma acc data attach(s.e)
    {
    }
#pragma acc exit data delete(a) attach(s.a) /* { dg-error ".attach. is not valid for" } */

#pragma acc enter data attach(scalar) /* { dg-error "expected pointer in .attach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc exit data detach(scalar) /* { dg-error "expected pointer in .detach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc enter data attach(s) /* { dg-error "expected pointer in .attach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc exit data detach(s) /* { dg-error "expected pointer in .detach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
  }

#pragma acc enter data attach(y[10])
#pragma acc exit data detach(y[10])
}
