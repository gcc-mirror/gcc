/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-overflow" } */

struct glyph
{
  long foo, bar, baz;
};

extern int fatal (char const *, int, int);

int
check_image_width (int width, int height)
{
  if ((((((0 * (0 * 2 + width) - 1) < 0) ? - (~ (0 * (0 * 2 + width) + 0) == -1) - ((((0 * (0 * 2 + width) + 1) << (sizeof ((0 * 2 + width) + 0) * 8 - 2)) - 1) * 2 + 1) : (0 * (0 * 2 + width) + 0))) < 0 ? (2 < 0 ? width < ((((0 * (0 * 2 + width) - 1) < 0) ? - (~ (0 * (0 * 2 + width) + 0) == -1) - ((((0 * (0 * 2 + width) + 1) << (sizeof ((0 * 2 + width) + 0) * 8 - 2)) - 1) * 2 + 1) : (0 * (0 * 2 + width) + 0))) - 2 : ((((0 * (0 * 2 + width) - 1) < 0) ? ((((0 * (0 * 2 + width) + 1) << (sizeof ((0 * 2 + width) + 0) * 8 - 2)) - 1) * 2 + 1) : (0 * (0 * 2 + width) - 1))) - 2 < width) : width < 0 ? 2 <= width + 2 : 2 < 0 ? width <= width + 2 : width + 2 < 2)
      || ((((((0 * (0 * height + (width + 2)) - 1) < 0) ? - (~ (0 * (0 * height + (width + 2)) + 0) == -1) - ((((0 * (0 * height + (width + 2)) + 1) << (sizeof ((0 * height + (width + 2)) + 0) * 8 - 2)) - 1) * 2 + 1) : (0 * (0 * height + (width + 2)) + 0))) == 0 && (((width + 2) < 0 && 0 < height) || (height < 0 && 0 < (width + 2)))) || (height < 0 ? ((width + 2) < 0 ? (width + 2) < ((((0 * (0 * height + (width + 2)) - 1) < 0) ? ((((0 * (0 * height + (width + 2)) + 1) << (sizeof ((0 * height + (width + 2)) + 0) * 8 - 2)) - 1) * 2 + 1) : (0 * (0 * height + (width + 2)) - 1))) / height : height == -1 ? 0 : ((((0 * (0 * height + (width + 2)) - 1) < 0) ? - (~ (0 * (0 * height + (width + 2)) + 0) == -1) - ((((0 * (0 * height + (width + 2)) + 1) << (sizeof ((0 * height + (width + 2)) + 0) * 8 - 2)) - 1) * 2 + 1) : (0 * (0 * height + (width + 2)) + 0))) / height < (width + 2)) : height == 0 ? 0 : ((width + 2) < 0 ? (width + 2) < ((((0 * (0 * height + (width + 2)) - 1) < 0) ? - (~ (0 * (0 * height + (width + 2)) + 0) == -1) - ((((0 * (0 * height + (width + 2)) + 1) << (sizeof ((0 * height + (width + 2)) + 0) * 8 - 2)) - 1) * 2 + 1) : (0 * (0 * height + (width + 2)) + 0))) / height : ((((0 * (0 * height + (width + 2)) - 1) < 0) ? ((((0 * (0 * height + (width + 2)) + 1) << (sizeof ((0 * height + (width + 2)) + 0) * 8 - 2)) - 1) * 2 + 1) : (0 * (0 * height + (width + 2)) - 1))) / height < (width + 2))))
      || ((9223372036854775807L < 18446744073709551615UL ? 9223372036854775807L : 18446744073709551615UL) / sizeof (struct glyph)
	  < (width + 2) * height))
    fatal ("screen size %dx%d too big", width, height);
}
