/* Cases which will be IMHO always invalid in OpenMP,
   just perhaps could have different diagnostic wording.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -fopenmp" } */

void
foo ()
{
 label1:
  for (int i = 0; i < 32; ++i)
    #pragma omp parallel for
    for (int j = 0; j < 32; ++j)
      {
	if (j == 31)
	  break label1;			/* { dg-error "break statement used with OpenMP for loop" } */
	else if (j == 30)
	  continue label1;		/* { dg-error "invalid branch to/from OpenMP structured block" } */
       label2:
	for (int k = 0; k < 32; ++k)
	  if (k == 31)
	    break label2;
	  else if (k == 30)
	    continue label2;
	  else if (k == 29)
	    break label1;		/* { dg-error "invalid branch to/from OpenMP structured block" } */
	  else if (k == 28)
	    continue label1;		/* { dg-error "invalid branch to/from OpenMP structured block" } */
      }
}

void
bar ()
{
 label1:
  #pragma omp parallel for
  for (int i = 0; i < 32; ++i)
    if (i == 31)
      break label1;			/* { dg-error "break' statement operand 'label1' does not refer to a named loop or 'switch'" } */
					/* { dg-error "break statement used with OpenMP for loop" "" { target *-*-* } .-1 } */
 label2:
  #pragma omp parallel for collapse(2)
  for (int i = 0; i < 32; ++i)
    for (int j = 0; j < 32; ++j)
      if (i == 31 && j == 31)
	break label2;			/* { dg-error "'break' statement operand 'label2' does not refer to a named loop or 'switch'" } */
					/* { dg-error "break statement used with OpenMP for loop" "" { target *-*-* } .-1 } */
      else if (i == 31 && j == 30)
	continue label2;		/* { dg-error "'continue' statement operand 'label2' does not refer to a named loop" } */
}

void
baz ()
{
 label1:
  [[omp::directive (parallel for)]]
  for (int i = 0; i < 32; ++i)
    if (i == 31)
      break label1;			/* { dg-error "break' statement operand 'label1' does not refer to a named loop or 'switch'" } */
					/* { dg-error "break statement used with OpenMP for loop" "" { target *-*-* } .-1 } */
 label2:
  [[omp::directive (parallel for, collapse(2))]]
  for (int i = 0; i < 32; ++i)
    for (int j = 0; j < 32; ++j)
      if (i == 31 && j == 31)
	break label2;			/* { dg-error "'break' statement operand 'label2' does not refer to a named loop or 'switch'" } */
					/* { dg-error "break statement used with OpenMP for loop" "" { target *-*-* } .-1 } */
      else if (i == 31 && j == 30)
	continue label2;		/* { dg-error "'continue' statement operand 'label2' does not refer to a named loop" } */
}

void
qux ()
{
 label1:
  #pragma omp parallel for collapse(2)
  for (int i = 0; i < 32; ++i)		/* { dg-error "not enough nested loops" } */
   label2:
    for (int j = 0; j < 32; ++j)
      if (j == 31)
	break label1;			/* { dg-error "'break' statement operand 'label1' does not refer to a named loop or 'switch'; did you mean 'label2'\\\?" } */
      else if (j == 30)
	continue label1;		/* { dg-error "'continue' statement operand 'label1' does not refer to a named loop; did you mean 'label2'\\\?" } */
      else if (j == 29)
	break label2;			/* This is IMHO invalid too and currently just diagnosed by the not enough nested loops.  */
}

void
garply ()
{
 label1:
  [[omp::directive (parallel for, collapse(2))]]
  for (int i = 0; i < 32; ++i)		/* { dg-error "not enough nested loops" } */
   label2:
    for (int j = 0; j < 32; ++j)
      if (j == 31)
	break label1;			/* { dg-error "'break' statement operand 'label1' does not refer to a named loop or 'switch'; did you mean 'label2'\\\?" } */
      else if (j == 30)
	continue label1;		/* { dg-error "'continue' statement operand 'label1' does not refer to a named loop; did you mean 'label2'\\\?" } */
      else if (j == 29)
	break label2;			/* This is IMHO invalid too and currently just diagnosed by the not enough nested loops.  */
}
