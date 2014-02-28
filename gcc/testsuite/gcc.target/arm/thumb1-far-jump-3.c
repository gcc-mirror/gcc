/* Catch reload ICE on target thumb1 with far jump optimization.
 * It is also a valid case for non-thumb1 target.  */

/* Add -mno-lra option as it is only reproducable with reload.  It will
   be removed after reload is completely removed.  */
/* { dg-options "-mno-lra -fomit-frame-pointer" } */
/* { dg-do compile } */

#define C      2
#define A      4
#define RGB  (C | A)
#define GRAY (A)

typedef unsigned long uint_32;
typedef unsigned char byte;
typedef byte        * bytep;

typedef struct ss
{
   uint_32 w;
   uint_32 r;
   byte c;
   byte b;
   byte p;
} info;

typedef info * infop;

void
foo(infop info, bytep row)
{
   uint_32 iw = info->w;
   if (info->c == RGB)
   {
      if (info->b == 8)
      {
         bytep sp = row + info->r;
         bytep dp = sp;
         byte save;
         uint_32 i;

         for (i = 0; i < iw; i++)
         {
            save = *(--sp);
            *(--dp) = *(--sp);
            *(--dp) = *(--sp);
            *(--dp) = *(--sp);
            *(--dp) = save;
         }
      }

      else
      {
         bytep sp = row + info->r;
         bytep dp = sp;
         byte save[2];
         uint_32 i;

         for (i = 0; i < iw; i++)
         {
            save[0] = *(--sp);
            save[1] = *(--sp);
            *(--dp) = *(--sp);
            *(--dp) = *(--sp);
            *(--dp) = *(--sp);
            *(--dp) = *(--sp);
            *(--dp) = *(--sp);
            *(--dp) = *(--sp);
            *(--dp) = save[0];
            *(--dp) = save[1];
         }
      }
   }
   else if (info->c == GRAY)
   {
      if (info->b == 8)
      {
         bytep sp = row + info->r;
         bytep dp = sp;
         byte save;
         uint_32 i;

         for (i = 0; i < iw; i++)
         {
            save = *(--sp);
            *(--dp) = *(--sp);
            *(--dp) = save;
         }
      }
      else
      {
         bytep sp = row + info->r;
         bytep dp = sp;
         byte save[2];
         uint_32 i;

         for (i = 0; i < iw; i++)
         {
            save[0] = *(--sp);
            save[1] = *(--sp);
            *(--dp) = *(--sp);
            *(--dp) = *(--sp);
            *(--dp) = save[0];
            *(--dp) = save[1];
         }
      }
   }
}
