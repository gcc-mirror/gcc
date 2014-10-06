/* PR target/13058 */
/* Origin: Lloyd Parkes <lloyd@must-have-coffee.gen.nz> */
/* Reduced testcase by  Falk Hueffner <falk@debian.org> */

/* Verify that the register allocator correctly aligns
   floating-point registers on SPARC64.  */

/* { dg-do compile } */
/* { dg-options "-O" } */

void Get16u();
typedef struct { int ThumbnailSize; } ImageInfo_t;

double ConvertAnyFormat(void)
{
  return 0;
}

void ProcessExifDir(ImageInfo_t *ImageInfoP, int NumDirEntries)
{
  unsigned int ThumbnailSize;

  for (; NumDirEntries;) {
    Get16u();
    switch (NumDirEntries) {
      case 0x0201:
      case 0x0202:
        ThumbnailSize = ConvertAnyFormat();
    }
  }

  ImageInfoP->ThumbnailSize = ThumbnailSize;
}
