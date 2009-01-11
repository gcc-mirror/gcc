/* PR middle-end/38616 */
/* { dg-do run } */
/* { dg-options "-O2 -fstack-protector" } */

#include <stdio.h> 

#define BUFFER "1234567890abcdefghijklmno"
int main (void)
{
  char buffer[1024]="";
  sprintf (buffer, "%s", BUFFER);
  return strcmp (buffer, BUFFER);
}
