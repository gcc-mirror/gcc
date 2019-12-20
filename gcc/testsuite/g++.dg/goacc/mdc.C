/* Test OpenACC's support for manual deep copy, including the attach
   and detach clauses.  */

void
t1 ()
{
  struct foo {
    int *a, *b, c, d, *e;
  } s;

  struct foo& rs = s;
  
  int *a, *z, scalar, **y;
  int* const &ra = a;
  int* const &rz = z;
  int& rscalar = scalar;
  int** const &ry = y;

#pragma acc enter data copyin(rs) detach(rz) /* { dg-error ".detach. is not valid for" } */
  {
#pragma acc data copy(rs.a[0:10]) copy(rz[0:10])
    {
      s.e = z;
#pragma acc parallel loop attach(rs.e) detach(rs.b) /* { dg-error ".detach. is not valid for" } */
      for (int i = 0; i < 10; i++)
        s.a[i] = s.e[i];

      a = s.e;
#pragma acc enter data attach(ra) detach(rs.c) /* { dg-error ".detach. is not valid for" } */
#pragma acc exit data detach(ra)
    }

#pragma acc enter data attach(rz[:5]) /* { dg-error "expected single pointer in .attach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc exit data detach(rz[:5]) /* { dg-error "expected single pointer in .detach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc enter data attach(rz[1:]) /* { dg-error "expected single pointer in .attach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc exit data detach(rz[1:]) /* { dg-error "expected single pointer in .detach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc enter data attach(rz[:]) /* { dg-error "expected single pointer in .attach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc exit data detach(rz[:]) /* { dg-error "expected single pointer in .detach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc enter data attach(rz[3]) /* { dg-error "expected pointer in .attach. clause" } */
#pragma acc exit data detach(rz[3]) /* { dg-error "expected pointer in .detach. clause" } */

#pragma acc acc enter data attach(rs.e)
#pragma acc exit data detach(rs.e) attach(rz) /* { dg-error ".attach. is not valid for" } */

#pragma acc data attach(rs.e)
    {
    }
#pragma acc exit data delete(ra) attach(rs.a) /* { dg-error ".attach. is not valid for" } */

#pragma acc enter data attach(rscalar) /* { dg-error "expected pointer in .attach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc exit data detach(rscalar) /* { dg-error "expected pointer in .detach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc enter data attach(rs) /* { dg-error "expected pointer in .attach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
#pragma acc exit data detach(rs) /* { dg-error "expected pointer in .detach. clause" } */
/* { dg-error "has no data movement clause" "" { target *-*-* } .-1 } */
  }

#pragma acc enter data attach(ry[10])
#pragma acc exit data detach(ry[10])
}
