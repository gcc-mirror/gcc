/* PR17412 - ICE in fold_const.c during parsing.
   fold would try to fold the operands of the break statement.  */
/* { dg-do compile } */
/* { dg-options "" } */
 

void foo ()
{
  for (;;)
    for (;;({break;}));
}

