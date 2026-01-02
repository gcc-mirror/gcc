/* Character conversion functions for the Algol 68 front-end.
   Copyright (C) 1999-2026 Free Software Foundation, Inc.
   Copyright (C) 2025 Jose E. Marchesi.

   The code in this file has been adapted from the unistr gnulib module,
   written by Bruno Haible.
  
   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "a68.h"

static int
u8_mbtoucr (uint32_t *puc, const uint8_t *s, size_t n)
{
  uint8_t c = *s;

  if (c < 0x80)
    {
      *puc = c;
      return 1;
    }
  else if (c >= 0xc2)
    {
      if (c < 0xe0)
        {
          if (n >= 2)
            {
              if ((s[1] ^ 0x80) < 0x40)
                {
                  *puc = ((unsigned int) (c & 0x1f) << 6)
                         | (unsigned int) (s[1] ^ 0x80);
                  return 2;
                }
              /* invalid multibyte character */
            }
          else
            {
              /* incomplete multibyte character */
              *puc = 0xfffd;
              return -2;
            }
        }
      else if (c < 0xf0)
        {
          if (n >= 2)
            {
              if ((s[1] ^ 0x80) < 0x40
                  && (c >= 0xe1 || s[1] >= 0xa0)
                  && (c != 0xed || s[1] < 0xa0))
                {
                  if (n >= 3)
                    {
                      if ((s[2] ^ 0x80) < 0x40)
                        {
                          *puc = ((unsigned int) (c & 0x0f) << 12)
                                 | ((unsigned int) (s[1] ^ 0x80) << 6)
                                 | (unsigned int) (s[2] ^ 0x80);
                          return 3;
                        }
                      /* invalid multibyte character */
                    }
                  else
                    {
                      /* incomplete multibyte character */
                      *puc = 0xfffd;
                      return -2;
                    }
                }
              /* invalid multibyte character */
            }
          else
            {
              /* incomplete multibyte character */
              *puc = 0xfffd;
              return -2;
            }
        }
      else if (c <= 0xf4)
        {
          if (n >= 2)
            {
              if ((s[1] ^ 0x80) < 0x40
                  && (c >= 0xf1 || s[1] >= 0x90)
                  && (c < 0xf4 || (/* c == 0xf4 && */ s[1] < 0x90)))
                {
                  if (n >= 3)
                    {
                      if ((s[2] ^ 0x80) < 0x40)
                        {
                          if (n >= 4)
                            {
                              if ((s[3] ^ 0x80) < 0x40)
                                {
                                  *puc = ((unsigned int) (c & 0x07) << 18)
                                         | ((unsigned int) (s[1] ^ 0x80) << 12)
                                         | ((unsigned int) (s[2] ^ 0x80) << 6)
                                         | (unsigned int) (s[3] ^ 0x80);
                                  return 4;
                                }
                              /* invalid multibyte character */
                            }
                          else
                            {
                              /* incomplete multibyte character */
                              *puc = 0xfffd;
                              return -2;
                            }
                        }
                      /* invalid multibyte character */
                    }
                  else
                    {
                      /* incomplete multibyte character */
                      *puc = 0xfffd;
                      return -2;
                    }
                }
              /* invalid multibyte character */
            }
          else
            {
              /* incomplete multibyte character */
              *puc = 0xfffd;
              return -2;
            }
        }
    }
  /* invalid multibyte character */
  *puc = 0xfffd;
  return -1;
}

/* Get the UCS code for the first character of a given UTF-8 string.  */

int
a68_u8_mbtouc (uint32_t *puc, const uint8_t *s, size_t n)
{
  uint8_t c = *s;

  if (c < 0x80)
    {
      *puc = c;
      return 1;
    }
  else if (c >= 0xc2)
    {
      if (c < 0xe0)
        {
          if (n >= 2)
            {
              if ((s[1] ^ 0x80) < 0x40)
                {
                  *puc = ((unsigned int) (c & 0x1f) << 6)
                         | (unsigned int) (s[1] ^ 0x80);
                  return 2;
                }
              /* invalid multibyte character */
            }
          else
            {
              /* incomplete multibyte character */
              *puc = 0xfffd;
              return 1;
            }
        }
      else if (c < 0xf0)
        {
          if (n >= 3)
            {
              if ((s[1] ^ 0x80) < 0x40
                  && (c >= 0xe1 || s[1] >= 0xa0)
                  && (c != 0xed || s[1] < 0xa0))
                {
                  if ((s[2] ^ 0x80) < 0x40)
                    {
                      *puc = ((unsigned int) (c & 0x0f) << 12)
                             | ((unsigned int) (s[1] ^ 0x80) << 6)
                             | (unsigned int) (s[2] ^ 0x80);
                      return 3;
                    }
                  /* invalid multibyte character */
                  *puc = 0xfffd;
                  return 2;
                }
              /* invalid multibyte character */
              *puc = 0xfffd;
              return 1;
            }
          else
            {
              *puc = 0xfffd;
              if (n == 1)
                {
                  /* incomplete multibyte character */
                  return 1;
                }
              else
                {
                  if ((s[1] ^ 0x80) < 0x40
                      && (c >= 0xe1 || s[1] >= 0xa0)
                      && (c != 0xed || s[1] < 0xa0))
                    {
                      /* incomplete multibyte character */
                      return 2;
                    }
                  else
                    {
                      /* invalid multibyte character */
                      return 1;
                    }
                }
            }
        }
      else if (c <= 0xf4)
        {
          if (n >= 4)
            {
              if ((s[1] ^ 0x80) < 0x40
                  && (c >= 0xf1 || s[1] >= 0x90)
                  && (c < 0xf4 || (/* c == 0xf4 && */ s[1] < 0x90)))
                {
                  if ((s[2] ^ 0x80) < 0x40)
                    {
                      if ((s[3] ^ 0x80) < 0x40)
                        {
                          *puc = ((unsigned int) (c & 0x07) << 18)
                                 | ((unsigned int) (s[1] ^ 0x80) << 12)
                                 | ((unsigned int) (s[2] ^ 0x80) << 6)
                                 | (unsigned int) (s[3] ^ 0x80);
                          return 4;
                        }
                      /* invalid multibyte character */
                      *puc = 0xfffd;
                      return 3;
                    }
                  /* invalid multibyte character */
                  *puc = 0xfffd;
                  return 2;
                }
              /* invalid multibyte character */
              *puc = 0xfffd;
              return 1;
            }
          else
            {
              *puc = 0xfffd;
              if (n == 1)
                {
                  /* incomplete multibyte character */
                  return 1;
                }
              else
                {
                  if ((s[1] ^ 0x80) < 0x40
                      && (c >= 0xf1 || s[1] >= 0x90)
                      && (c < 0xf4 || (/* c == 0xf4 && */ s[1] < 0x90)))
                    {
                      if (n == 2)
                        {
                          /* incomplete multibyte character */
                          return 2;
                        }
                      else
                        {
                          if ((s[2] ^ 0x80) < 0x40)
                            {
                              /* incomplete multibyte character */
                              return 3;
                            }
                          else
                            {
                              /* invalid multibyte character */
                              return 2;
                            }
                        }
                    }
                  else
                    {
                      /* invalid multibyte character */
                      return 1;
                    }
                }
            }
        }
    }
  /* invalid multibyte character */
  *puc = 0xfffd;
  return 1;
}

/* Encode a given UCS code in UTF-8.  */

int
a68_u8_uctomb (uint8_t *s, uint32_t uc, ptrdiff_t n)
{
  if (uc < 0x80)
    {
      if (n > 0)
        {
          s[0] = uc;
          return 1;
        }
      /* else return -2, below.  */
    }
  else
    {
      int count;

      if (uc < 0x800)
        count = 2;
      else if (uc < 0x10000)
        {
          if (uc < 0xd800 || uc >= 0xe000)
            count = 3;
          else
            return -1;
        }
      else if (uc < 0x110000)
        count = 4;
      else
        return -1;

      if (n >= count)
        {
          switch (count) /* note: code falls through cases! */
            {
            case 4: s[3] = 0x80 | (uc & 0x3f); uc = uc >> 6; uc |= 0x10000;
              gcc_fallthrough ();
            case 3: s[2] = 0x80 | (uc & 0x3f); uc = uc >> 6; uc |= 0x800;
              gcc_fallthrough ();
            case 2: s[1] = 0x80 | (uc & 0x3f); uc = uc >> 6; uc |= 0xc0;
          /*case 1:*/ s[0] = uc;
            }
          return count;
        }
    }
  return -2;
}

/* Convert UTF-8 to UTF-32/UCS-4  */

uint32_t *
a68_u8_to_u32 (const uint8_t *s, size_t n, uint32_t *resultbuf, size_t *lengthp)
{
  const uint8_t *s_end = s + n;
  /* Output string accumulator.  */
  uint32_t *result;
  size_t allocated;
  size_t length;

  if (resultbuf != NULL)
    {
      result = resultbuf;
      allocated = *lengthp;
    }
  else
    {
      result = NULL;
      allocated = 0;
    }
  length = 0;
  /* Invariants:
     result is either == resultbuf or == NULL or malloc-allocated.
     If length > 0, then result != NULL.  */

  while (s < s_end)
    {
      uint32_t uc;
      int count;

      /* Fetch a Unicode character from the input string.  */
      count = u8_mbtoucr (&uc, s, s_end - s);
      if (count < 0)
        {
          if (!(result == resultbuf || result == NULL))
            free (result);
          errno = EILSEQ;
          return NULL;
        }
      s += count;

      /* Store it in the output string.  */
      if (length + 1 > allocated)
        {
          uint32_t *memory;

          allocated = (allocated > 0 ? 2 * allocated : 12);
          if (length + 1 > allocated)
            allocated = length + 1;
          if (result == resultbuf || result == NULL)
            memory = (uint32_t *) xmalloc (allocated * sizeof (uint32_t));
          else
            memory =
              (uint32_t *) xrealloc (result, allocated * sizeof (uint32_t));

          if (memory == NULL)
            {
              if (!(result == resultbuf || result == NULL))
                free (result);
              errno = ENOMEM;
              return NULL;
            }
          if (result == resultbuf && length > 0)
            memcpy ((char *) memory, (char *) result,
                    length * sizeof (uint32_t));
          result = memory;
        }
      result[length++] = uc;
    }

  if (length == 0)
    {
      if (result == NULL)
        {
          /* Return a non-NULL value.  NULL means error.  */
          result = (uint32_t *) xmalloc (sizeof (uint32_t));
          if (result == NULL)
            {
              errno = ENOMEM;
              return NULL;
            }
        }
    }
  else if (result != resultbuf && length < allocated)
    {
      /* Shrink the allocated memory if possible.  */
      uint32_t *memory;

      memory = (uint32_t *) xrealloc (result, length * sizeof (uint32_t));
      if (memory != NULL)
        result = memory;
    }

  *lengthp = length;
  return result;
}
