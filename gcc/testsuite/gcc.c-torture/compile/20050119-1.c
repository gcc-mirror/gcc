void write_char(char);
int len(char*);
void f(char *a)
{
  int col = 0;
  int i;
  void wchar(char c)
  {
    if (c == '\t')
     {
      do {
       wchar(' ');
      } while ((col%8)!=0);
     }
    else
     {
       write_char (c);
       col++;
     }
  }
  for(i =0;i<len(a);i++)
  {
   wchar(*a);
  }
}
