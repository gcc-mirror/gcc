typedef unsigned char uch;
uch inbuf[8];
extern unsigned insize;
unsigned inptr; 
int to_stdout = 0;
int force = 0;
extern int fill_inbuf (int);
int get_method(int in)
{
  char magic[2];
  if (force && to_stdout)
    magic[0] = (char)(inptr < insize ? inbuf[inptr++] : fill_inbuf(1));
  else
    magic[1] = (char)(inptr < insize ? inbuf[inptr++] : fill_inbuf(0));
}
int main()
{
  return 0;
}

