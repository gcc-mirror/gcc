extern void abort (void);
unsigned short c = 0x8000;
int main()
{
  if ((c-0x8000) < 0 || (c-0x8000) > 0x7fff)
    abort();
  return 0;
}
