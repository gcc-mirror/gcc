/* { dg-do compile } */
/* { dg-options "-mthumb -mcpu=cortex-m4 -fprefetch-loop-arrays -O2" }  */

typedef struct genxWriter_rec * genxWriter;
typedef unsigned char * utf8;
typedef const unsigned char * constUtf8;

int genxScrubText(genxWriter w, constUtf8 in, utf8 out)
{
  int problems = 0;
  constUtf8 last = in;

  while (*in)
  {
    int c = genxNextUnicodeChar(&in);
    if (c == -1)
    {
      problems++;
      last = in;
      continue;
    }

    if (!isXMLChar(w, c))
    {
      problems++;
      last = in;
      continue;
    }

    while (last < in)
      *out++ = *last++;
  }
  *out = 0;
  return problems;
}
