/* { dg-do compile } */
/* { dg-options "-w" } */

extern int strlen(const char *);
void WriteTextDots(int len);

void OnDisplay(char * string)
{
  if (!string)
    string = "(none)";
  WriteTextDots(strlen(string));
}

