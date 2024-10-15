/* N3355 - Named loops.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

void
foo (int x)
{
 label1:
  for (int i = 0; i < 16; ++i)
   another_label1:
    for (int j = 0; j < 16; ++j)
      break label2;		/* { dg-error "'break' statement operand 'label2' does not refer to a named loop or 'switch'; did you mean 'label1'\\\?" } */
  for (int i = 0; i < 16; ++i)
    break label3;		/* { dg-error "'break' statement operand 'label3' does not refer to a named loop or 'switch'" } */
 label4:			/* { dg-message "'switch' name defined here" } */
  switch (x)
    {
    case 0:
      for (int i = 0; i < 16; ++i)
	continue label5;	/* { dg-error "'continue' statement operand 'label5' does not refer to a named loop" } */
      break label4;
    case 1:
      for (int i = 0; i < 16; ++i)
	continue label4;	/* { dg-error "'continue' statement operand 'label4' refers to a named 'switch'" } */
    }
 label6:
  for (int i = 0; i < 16; ++i)
    continue label7;		/* { dg-error "'continue' statement operand 'label7' does not refer to a named loop; did you mean 'label6'\\\?" } */
 label2:
  for (int i = 0; i < 16; ++i)
    ;
 label8:;
  for (int i = 0; i < 16; ++i)
    break label8;		/* { dg-error "'break' statement operand 'label8' does not refer to a named loop or 'switch'" } */
 label9:;
  for (int i = 0; i < 16; ++i)
    continue label9;		/* { dg-error "'continue' statement operand 'label9' does not refer to a named loop" } */
 label10:
  ;
  switch (x)
    {
    case 0:
      break label10;		/* { dg-error "'break' statement operand 'label10' does not refer to a named loop or 'switch'" } */
    }
}
