/* { dg-do compile } */
/* { dg-options "-O2" } */

void pm_message(void);
struct CmdlineInfo {
  _Bool wantCrop[4];
  unsigned int margin;
};
typedef struct {
  unsigned int removeSize;
} CropOp;
typedef struct {
  CropOp op[4];
} CropSet;
static void divideAllBackgroundIntoBorders(unsigned int const totalSz,
                                           _Bool const wantCropSideA,
                                           _Bool const wantCropSideB,
                                           unsigned int const wantMargin,
                                           unsigned int *const sideASzP,
                                           unsigned int *const sideBSzP) {
  unsigned int sideASz, sideBSz;
  if (wantCropSideA && wantCropSideB)
  {
    sideASz = totalSz / 2;
    if (wantMargin)
      sideBSz = totalSz - sideASz;
  }
  else if (wantCropSideB)
  {
    sideBSz = 0;
  }
  *sideASzP = sideASz;
  *sideBSzP = sideBSz;
}
static CropOp oneSideCrop(_Bool const wantCrop, unsigned int const borderSz,
                          unsigned int const margin) {
  CropOp retval;
  if (wantCrop)
  {
    if (borderSz >= margin)
      retval.removeSize = borderSz - margin;
    else
      retval.removeSize = 0;
  }
  return retval;
}
struct CmdlineInfo cmdline1;
void f(int rows, int cols) {
  struct CmdlineInfo cmdline0 = cmdline1;
  CropSet crop;
  struct CmdlineInfo cmdline = cmdline0;
  CropSet retval;
  unsigned int leftBorderSz, rghtBorderSz;
  unsigned int topBorderSz, botBorderSz;
  divideAllBackgroundIntoBorders(cols, cmdline.wantCrop[0],
                                 cmdline.wantCrop[1], cmdline.margin > 0,
                                 &leftBorderSz, &rghtBorderSz);
  divideAllBackgroundIntoBorders(rows, cmdline.wantCrop[2],
                                 cmdline.wantCrop[3], cmdline.margin > 0,
                                 &topBorderSz, &botBorderSz);
  retval.op[0] =
      oneSideCrop(cmdline.wantCrop[0], leftBorderSz, cmdline.margin);
  retval.op[1] =
      oneSideCrop(cmdline.wantCrop[1], rghtBorderSz, cmdline.margin);
  retval.op[2] =
      oneSideCrop(cmdline.wantCrop[2], topBorderSz, cmdline.margin);
  retval.op[3] =
      oneSideCrop(cmdline.wantCrop[3], botBorderSz, cmdline.margin);
  crop = retval;
  unsigned int i = 0;
  for (i = 0; i < 4; ++i)
  {
    if (crop.op[i].removeSize == 0)
      pm_message();
  }
}
