/* { dg-do compile } */

int
main ()
{
  int i, *a, b;

#pragma acc parallel loop tile (10)
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile (*)
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile (10, *)
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile (10, *, i)
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile // { dg-error "expected '\\\('" }
  for (i = 0; i < 100; i++)
    ;  

#pragma acc parallel loop tile () // { dg-error "" }
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile (,1) // { dg-error "" }
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile (,,) // { dg-error "" }
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile (1.1) // { dg-error "'tile' value must be integral" }
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile (-3) // { dg-warning "'tile' value must be positive" }
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile (10,-3) // { dg-warning "'tile' value must be positive" }
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile (-100,10,5) // { dg-warning "'tile' value must be positive" }
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile (1,2.0,true) // { dg-error "" }
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile (*a, 1)
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile (1, *a, b)
  for (i = 0; i < 100; i++)
    ;

#pragma acc parallel loop tile (b, 1, *a)
  for (i = 0; i < 100; i++)
    ;

  return 0;
}
