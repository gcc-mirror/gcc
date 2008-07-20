typedef unsigned int u32;

static const u32 deadfish = 0xdeadf155;

static const u32 cfb_tab8_be[] = {
    0x00000000,0x000000ff,0x0000ff00,0x0000ffff,
    0x00ff0000,0x00ff00ff,0x00ffff00,0x00ffffff,
    0xff000000,0xff0000ff,0xff00ff00,0xff00ffff,
    0xffff0000,0xffff00ff,0xffffff00,0xffffffff
};

static const u32 cfb_tab8_le[] = {
    0x00000000,0xff000000,0x00ff0000,0xffff0000,
    0x0000ff00,0xff00ff00,0x00ffff00,0xffffff00,
    0x000000ff,0xff0000ff,0x00ff00ff,0xffff00ff,
    0x0000ffff,0xff00ffff,0x00ffffff,0xffffffff
};

static const u32 cfb_tab16_be[] = {
    0x00000000, 0x0000ffff, 0xffff0000, 0xffffffff
};

static const u32 cfb_tab16_le[] = {
    0x00000000, 0xffff0000, 0x0000ffff, 0xffffffff
};

static const u32 cfb_tab32[] = {
 0x00000000, 0xffffffff
};






const u32 *xxx(int bpp)
{
 const u32 *tab;

if (0) return &deadfish;

 switch (bpp) {
 case 8:
  tab = cfb_tab8_be;
  break;
 case 16:
  tab = cfb_tab16_be;
  break;
 case 32:
 default:
  tab = cfb_tab32;
  break;
 }

 return tab;
}

int main(void)
{
  const u32 *a = xxx(8);
  int b = a[0];
  if (b != cfb_tab8_be[0])
    __builtin_abort ();
  return 0;
}
