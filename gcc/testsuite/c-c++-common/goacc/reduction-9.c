/* Exercise invalid reductions on array and struct members.  */

void
test_parallel ()
{
  struct {
    int a;
    float b[5];
  } s1, s2[10];

  int i;
  double z[100];

#pragma acc parallel reduction(+:s1.a) /* { dg-error "expected '\\\)' before '\\\.' token" } */
  for (i = 0; i < 10; i++)
    s1.a += 1;

#pragma acc parallel reduction(+:s1.b[3]) /* { dg-error "expected '\\\)' before '\\\.' token" } */
  for (i = 0; i < 10; i++)
    s1.b[3] += 1;

#pragma acc parallel reduction(+:s2[2].a) /* { dg-error "expected '\\\)' before '\\\[' token" } */
  for (i = 0; i < 10; i++)
    s2[2].a += 1;

#pragma acc parallel reduction(+:s2[3].b[4]) /* { dg-error "expected '\\\)' before '\\\[' token" } */
  for (i = 0; i < 10; i++)
    s2[3].b[4] += 1;

#pragma acc parallel reduction(+:z[5]) /* { dg-error "expected '\\\)' before '\\\[' token" } */
  for (i = 0; i < 10; i++)
    z[5] += 1;
}

void
test_combined ()
{
  struct {
    int a;
    float b[5];
  } s1, s2[10];

  int i;
  double z[100];

#pragma acc parallel loop reduction(+:s1.a) /* { dg-error "expected '\\\)' before '\\\.' token" } */
  for (i = 0; i < 10; i++)
    s1.a += 1;

#pragma acc parallel loop reduction(+:s1.b[3]) /* { dg-error "expected '\\\)' before '\\\.' token" } */
  for (i = 0; i < 10; i++)
    s1.b[3] += 1;

#pragma acc parallel loop reduction(+:s2[2].a) /* { dg-error "expected '\\\)' before '\\\[' token" } */
  for (i = 0; i < 10; i++)
    s2[2].a += 1;

#pragma acc parallel loop reduction(+:s2[3].b[4]) /* { dg-error "expected '\\\)' before '\\\[' token" } */
  for (i = 0; i < 10; i++)
    s2[3].b[4] += 1;

#pragma acc parallel loop reduction(+:z[5]) /* { dg-error "expected '\\\)' before '\\\[' token" } */
  for (i = 0; i < 10; i++)
    z[5] += 1;

}

void
test_loops ()
{
  struct {
    int a;
    float b[5];
  } s1, s2[10];

  int i;
  double z[100];

#pragma acc parallel
  {
#pragma acc loop reduction(+:s1.a) /* { dg-error "expected '\\\)' before '\\\.' token" } */
  for (i = 0; i < 10; i++)
    s1.a += 1;

#pragma acc loop reduction(+:s1.b[3]) /* { dg-error "expected '\\\)' before '\\\.' token" } */
  for (i = 0; i < 10; i++)
    s1.b[3] += 1;

#pragma acc loop reduction(+:s2[2].a) /* { dg-error "expected '\\\)' before '\\\[' token" } */
  for (i = 0; i < 10; i++)
    s2[2].a += 1;

#pragma acc loop reduction(+:s2[3].b[4]) /* { dg-error "expected '\\\)' before '\\\[' token" } */
  for (i = 0; i < 10; i++)
    s2[3].b[4] += 1;

#pragma acc loop reduction(+:z[5]) /* { dg-error "expected '\\\)' before '\\\[' token" } */
  for (i = 0; i < 10; i++)
    z[5] += 1;
  }
}

int
main ()
{
  test_parallel ();
  test_combined ();
  test_loops ();

  return 0;
}
