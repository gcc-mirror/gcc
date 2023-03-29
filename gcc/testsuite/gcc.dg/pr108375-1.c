/* PR 108375
 * { dg-do compile }
 * { dg-options "" }
 * */

void
f (int a)
{
  goto x;	/* { dg-error "jump into scope of identifier with variably modified type" } */
  struct { char (*p)[a]; } B;
  x : ;
}


