/* { dg-do compile } */
/* { do-options "-O1" } */

int x;
void andsi3(void) { x &= ~(1 << 16); }
void iorsi3(void) { x |=  (1 << 16); }
void xorsi3(void) { x ^=  (1 << 16); }
