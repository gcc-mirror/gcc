void abort (void);
void exit (int);

   unsigned long bad(int reg, unsigned long inWord)
   {
       union {
           unsigned long word;
           unsigned char byte[4];
       } data;

       data.word = inWord;
       data.byte[reg] = 0;

       return data.word;
   }

int
main(void)
{
  /* XXX This test could be generalized.  */
  if (sizeof (long) != 4)
    exit (0);

  if (bad (0, 0xdeadbeef) == 0xdeadbeef)
    abort ();
  exit (0);
}
