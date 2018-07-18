/* { dg-do compile } */
/* This is like pr84682-1.c, but with an extra memory constraint, to
   check that we don't disable process_address altogether just because
   of the disabled address constraint.  */

void b(char a) {
        asm("" : : "pmir" (a));
}
