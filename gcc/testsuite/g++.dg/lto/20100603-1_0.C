/* { dg-lto-do link } */
/* { dg-extra-ld-options "-Wno-lto-type-mismatch" } */

extern "C" {
    typedef struct {} CvImage;
    extern CvImage* Cv_ImageNew(void);
}
void __attribute__((noinline,noclone))
_Raytrace(CvImage* LImage) { __asm volatile (""); }
int main(int LArgC, char** LArgV) 
{
  CvImage* LImage = Cv_ImageNew();
  _Raytrace(LImage);
}

