/* PR c/32061
   { dg-do compile } 
   { dg-options "-Wlogical-op -Wall -Wextra" }
*/
#define FORCE   1
#define FLAG    1
int func (int resp, int flags)
{
  return (resp && (FORCE || (FLAG & flags)));
}
