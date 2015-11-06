int t;
#pragma omp threadprivate (t)

void
foo (int y, short z)
{
  int x;
  #pragma omp target teams map (from: x)
  #pragma omp distribute simd linear (x : 2)
  for (x = 0; x < 64; x += 2)
    ;
  #pragma omp target teams map (from: x)
  #pragma omp distribute parallel for simd linear (x)
  for (x = 0; x < 64; x++)
    ;
  #pragma omp target teams map (tofrom: y)
  #pragma omp distribute simd linear (y : 2)	/* { dg-error ".linear. clause for variable other than loop iterator specified on construct combined with .distribute." } */
  for (x = 0; x < 64; x += 2)
    y += 2;
  #pragma omp target teams map (tofrom: z)
  #pragma omp distribute parallel for simd linear (z)	/* { dg-error ".linear. clause for variable other than loop iterator specified on construct combined with .distribute." } */
  for (x = 0; x < 64; x++)
    z++;
  #pragma omp target teams map (tofrom: z)
  #pragma omp distribute parallel for linear (z: 4)	/* { dg-error ".linear. is not valid for .#pragma omp distribute parallel for." } */
  for (x = 0; x < 64; x++)
    z += 4;
  #pragma omp target map (from: x)
  #pragma omp teams distribute simd linear (x : 2)
  for (x = 0; x < 64; x += 2)
    ;
  #pragma omp target map (from: x)
  #pragma omp teams distribute parallel for simd linear (x)
  for (x = 0; x < 64; x++)
    ;
  #pragma omp target map (tofrom: y)
  #pragma omp teams distribute simd linear (y : 2)	/* { dg-error ".linear. clause for variable other than loop iterator specified on construct combined with .distribute." } */
  for (x = 0; x < 64; x += 2)
    y += 2;
  #pragma omp target map (tofrom: z)
  #pragma omp teams distribute parallel for simd linear (z)	/* { dg-error ".linear. clause for variable other than loop iterator specified on construct combined with .distribute." } */
  for (x = 0; x < 64; x++)
    z++;
  #pragma omp target map (tofrom: z)
  #pragma omp teams distribute parallel for linear (z: 4)	/* { dg-error ".linear. is not valid for .#pragma omp teams distribute parallel for." } */
  for (x = 0; x < 64; x++)
    z += 4;
  #pragma omp target parallel copyin (t)	/* { dg-error ".copyin. is not valid for .#pragma omp target parallel." } */
    ;
  #pragma omp target parallel for copyin (t)	/* { dg-error ".copyin. is not valid for .#pragma omp target parallel for." } */
  for (x = 0; x < 64; x++)
    ;
  #pragma omp target parallel for simd copyin (t)	/* { dg-error ".copyin. is not valid for .#pragma omp target parallel for simd." } */
  for (x = 0; x < 64; x++)
    ;
  #pragma omp target teams
  #pragma omp distribute parallel for ordered		/* { dg-error ".ordered. is not valid for .#pragma omp distribute parallel for." } */
  for (x = 0; x < 64; x++)
    {
      #pragma omp ordered	/* { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" } */
      ;
    }
  #pragma omp target teams
  #pragma omp distribute parallel for simd ordered	/* { dg-error ".ordered. is not valid for .#pragma omp distribute parallel for simd." } */
  for (x = 0; x < 64; x++)
    {
      #pragma omp ordered simd, threads
      ;
    }
  #pragma omp target
  #pragma omp teams distribute parallel for ordered		/* { dg-error ".ordered. is not valid for .#pragma omp teams distribute parallel for." } */
  for (x = 0; x < 64; x++)
    {
      #pragma omp ordered	/* { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" } */
      ;
    }
  #pragma omp target
  #pragma omp teams distribute parallel for simd ordered	/* { dg-error ".ordered. is not valid for .#pragma omp teams distribute parallel for simd." } */
  for (x = 0; x < 64; x++)
    {
      #pragma omp ordered simd, threads
      ;
    }
  #pragma omp target teams distribute parallel for ordered		/* { dg-error ".ordered. is not valid for .#pragma omp target teams distribute parallel for." } */
  for (x = 0; x < 64; x++)
    {
      #pragma omp ordered	/* { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" } */
      ;
    }
  #pragma omp target teams distribute parallel for simd ordered	/* { dg-error ".ordered. is not valid for .#pragma omp target teams distribute parallel for simd." } */
  for (x = 0; x < 64; x++)
    {
      #pragma omp ordered simd, threads
      ;
    }
  #pragma omp simd
  for (x = 0; x < 64; x++)
    {
      #pragma omp ordered threads simd		/* { dg-error ".ordered simd threads. must be closely nested inside of .for simd. region" } */
      ;
    }
  #pragma omp for
  for (x = 0; x < 64; x++)
    {
      #pragma omp simd
      for (y = 0; y < 16; y++)
	{
	  #pragma omp ordered simd threads	/* { dg-error ".ordered simd threads. must be closely nested inside of .for simd. region" } */
	  ;
	}
    }
  #pragma omp for simd
  for (x = 0; x < 64; x++)
    {
      #pragma omp ordered threads simd
      ;
    }
}
