/* PR17412
   fold-const would try to fold the operands of the break statement.  */
/* { dg-do compile } */
 

void foo ()
{
  for (;;)
    for (;;({break;}));
}

