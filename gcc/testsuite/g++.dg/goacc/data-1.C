void
foo (int &a, int (&b)[100], int &n)
{
#pragma acc enter data copyin (a, b) async wait
#pragma acc enter data create (b[20:30]) async wait
#pragma acc enter data (a) /* { dg-error "expected '#pragma acc' clause before '\\\(' token" } */
#pragma acc enter data create (b(1:10)) /* { dg-error "expected '\\\)' before '\\\(' token" } */
#pragma acc exit data delete (a) if (0)
#pragma acc exit data copyout (b) if (a)
#pragma acc exit data delete (b)
#pragma acc enter /* { dg-error "expected 'data' in" } */
#pragma acc exit /* { dg-error "expected 'data' in" } */
#pragma acc enter data /* { dg-error "has no data movement clause" } */
#pragma acc exit data /* { dg-error "has no data movement clause" } */
#pragma acc enter Data /* { dg-error "invalid pragma before" } */
#pragma acc exit copyout (b) /* { dg-error "invalid pragma before" } */
}

template<typename T>
void
foo (T &a, T (&b)[100], T &n)
{
#pragma acc enter data copyin (a, b) async wait
#pragma acc enter data create (b[20:30]) async wait
#pragma acc enter data (a) /* { dg-error "expected '#pragma acc' clause before '\\\(' token" } */
#pragma acc enter data create (b(1:10)) /* { dg-error "expected '\\\)' before '\\\(' token" } */
#pragma acc exit data delete (a) if (0)
#pragma acc exit data copyout (b) if (a)
#pragma acc exit data delete (b)
#pragma acc enter /* { dg-error "expected 'data' in" } */
#pragma acc exit /* { dg-error "expected 'data' in" } */
#pragma acc enter data /* { dg-error "has no data movement clause" } */
#pragma acc exit data /* { dg-error "has no data movement clause" } */
#pragma acc enter Data /* { dg-error "invalid pragma before" } */
#pragma acc exit copyout (b) /* { dg-error "invalid pragma before" } */
}

/* { dg-error "has no data movement clause" "" { target *-*-* } 6 } */
/* { dg-error "has no data movement clause" "" { target *-*-* } 25 } */
