/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection" } */

int flag;
void open();
int getChar();
typedef enum { QUOTE } CharType;
typedef enum { UNQ } State;
CharType getCharType();
void expand() {
  open();
  if (flag)
    return;
  int ch = getChar();
  State nextState = getCharType();
  if (nextState)
    while (ch)
      ;
}
