/* N3355 - Named loops.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -Wall" } */

void
foo (int x)
{
 lab0:
  switch (x)
    {
    case 1:
      ++x;
      [[fallthrough]];
    lab1:			/* { dg-warning "label 'lab1' defined but not used" } */
    case 2:
      [[fallthrough]];
    case 3:
    lab2:
      for (int i = 0; i < 4; ++i)
	if (i == 0)
	  continue lab2;
	else if (i == 1)
	  continue lab1;	/* { dg-error "'continue' statement operand 'lab1' does not refer to a named loop; did you mean 'lab2'\\\?" } */
	else if (x == 2)
	  break lab1;		/* { dg-error "'break' statement operand 'lab1' does not refer to a named loop or 'switch'; did you mean 'lab2'\\\?" } */
	else
	  break lab0;
      break;
    default:
      break;
    }
}
