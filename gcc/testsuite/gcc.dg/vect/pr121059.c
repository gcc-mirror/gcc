/* { dg-do compile } */
/* { dg-additional-options "-O3 --param vect-partial-vector-usage=1" } */
/* { dg-additional-options "-march=x86-64-v4" { target avx512f } } */

typedef struct {
  long left, right, top, bottom;
} MngBox;
typedef struct {
  MngBox object_clip[6];
  char exists[256], frozen[];
} MngReadInfo;
MngReadInfo mng_info;

long ReadMNGImage_i;

void ReadMNGImage(int ReadMNGImage_i)
{
  for (; ReadMNGImage_i < 256; ReadMNGImage_i++)
    if (mng_info.exists[ReadMNGImage_i] && mng_info.frozen[ReadMNGImage_i])
      mng_info.object_clip[ReadMNGImage_i].left =
          mng_info.object_clip[ReadMNGImage_i].right =
              mng_info.object_clip[ReadMNGImage_i].top =
                  mng_info.object_clip[ReadMNGImage_i].bottom = 0;
}
