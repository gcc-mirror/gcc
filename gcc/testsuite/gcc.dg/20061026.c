/* { dg-do compile } */
/* { dg-options "-O1" } */

/* This testcase failed on s390.  The frame size for function f will be
   exactly 32768 bytes.  The back end has to recognize that this is to
   large for a 16bit constant and therefore should reserve the literal
   pool base pointer.  */

void g ();

int f () {
  char a[32608];

  g (a);
}
