/* { dg-do compile } */
/* { dg-options "-O3 -w" } */

void pm_message (int);
void _setjmp (void);
int *findOrAddBackgroundInPalette_palette_pnm;
static void findOrAddBackgroundInPalette(unsigned *paletteSizeP,
                                    int *backgroundIndexP) {
  if (*paletteSizeP) {
    *backgroundIndexP = (*paletteSizeP)++;
    pm_message(0);
  }
  pm_message(findOrAddBackgroundInPalette_palette_pnm[*backgroundIndexP]);
}
void computeColorMap(int *backgroundIndexP) {
  unsigned paletteSize;
  findOrAddBackgroundInPalette(&paletteSize, backgroundIndexP);
}
int main() {
  unsigned backgroundIndex;
  _setjmp();
  computeColorMap(&backgroundIndex);
}
