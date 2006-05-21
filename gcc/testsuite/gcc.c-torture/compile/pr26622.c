/* PR middle-end/26622
   fold_ternary used to create a tree with mismatching types, causing
   (const_int 128) to appear in QImode rtx.  */

unsigned char g;

unsigned long long
foo (void)
{
  return ((long long) ((g & 0x80) != 0)) << 7;
}
