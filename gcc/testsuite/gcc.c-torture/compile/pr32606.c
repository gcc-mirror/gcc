int inb(int);
void is870(unsigned int wkport, unsigned char j)
{
 unsigned int tmport;
 unsigned char i;
 for (i = 0; i < 16; i++)
 {
  tmport = wkport + 0x18;
  tmport += 0x07;
  while ((inb(tmport) & 0x80) == 0)
  {
   if ((inb(tmport) & 0x01) != 0)
   {
    tmport -= 0x06;
    tmport += 0x06;
   }
  }
  tmport = wkport + 0x14;
  tmport += 0x04;
  tmport += 0x07;
widep_in1:
  if ((j & 0x01) != 0)
  {
   tmport -= 0x06;
   tmport += 0x06;
   goto widep_in1;
  }
  while ((inb(tmport) & 0x80) == 0) {}
 }
}
