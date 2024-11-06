/* Cases which perhaps could be valid in OpenMP one day, but aren't
   accepted right now.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -fopenmp" } */

void
foo ()
{
 label1:
  #pragma omp parallel for
  for (int i = 0; i < 32; ++i)
    if (i == 31)
      continue label1;			/* { dg-error "'continue' statement operand 'label1' does not refer to a named loop" } */
    else
      {
       label2:
	for (int j = 0; j < 32; ++j)
	  if (j == 31)
	    continue label2;
	  else if (j == 30)
	    break label2;
	  else if (j == 29)
	    continue label1;		/* { dg-error "'continue' statement operand 'label1' does not refer to a named loop; did you mean 'label2'\\\?" } */
      }
}

void
bar ()
{
 label1:
  [[omp::directive (parallel for)]]
  for (int i = 0; i < 32; ++i)
    if (i == 31)
      continue label1;			/* { dg-error "'continue' statement operand 'label1' does not refer to a named loop" } */
    else
      {
       label2:
	for (int j = 0; j < 32; ++j)
	  if (j == 31)
	    continue label2;
	  else if (j == 30)
	    break label2;
	  else if (j == 29)
	    continue label1;		/* { dg-error "'continue' statement operand 'label1' does not refer to a named loop; did you mean 'label2'\\\?" } */
      }
}

void
baz ()
{
 label1:
  #pragma omp parallel for collapse(2)
  for (int i = 0; i < 32; ++i)		/* { dg-error "not enough nested loops" } */
   label2:
    for (int j = 0; j < 32; ++j)
     label3:
      for (int k = 0; k < 32; ++k)
	if (k == 31)
	  continue label3;
	else if (k == 30)
	  break label3;
	else if (k == 29)
	  continue label2;
}

void
qux ()
{
 label1:
  [[omp::directive (parallel for, collapse(2))]]
  for (int i = 0; i < 32; ++i)		/* { dg-error "not enough nested loops" } */
   label2:
    for (int j = 0; j < 32; ++j)
     label3:
      for (int k = 0; k < 32; ++k)
	if (k == 31)
	  continue label3;
	else if (k == 30)
	  break label3;
	else if (k == 29)
	  continue label2;
}
