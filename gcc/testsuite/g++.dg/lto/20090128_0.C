// { dg-lto-do assemble }
// { dg-lto-options {{-fpreprocessed -O2 -fwhopr -funsigned-char}} }
typedef unsigned char uint8;
extern const uint8 array[256];
static inline bool
g (unsigned char c)
{
  return array[c] & 0x80;
}

class Class1
{
  static bool f1 (char **dst, const char *end, char c);
  static bool f2 (const char *map, const char **src, char **dst,
		 const char *end);
  static bool f3 (const char *src, char *dst, const char *end);
};

enum JTipL
{
  KXHR8 = 0, KXNU3, KX_HASH, KXYYZ, KXFI9, KXX3, KXAFA, KXV4Z, KXZ11,
};

static const char
  p9t42[256] = { KXYYZ, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXAFA, KXX3, KX_HASH, KXAFA,
      KXFI9, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXV4Z, KXAFA, KXAFA,
      KXAFA, KXV4Z, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA,
      KXAFA, KXAFA, KXAFA, KXAFA, KXHR8, KXX3, KXV4Z, KXX3, KXNU3,
      KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA,
      KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA,
      KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA,
      KXX3, KXX3, KXX3, KXX3, KXAFA, KXX3, KXAFA, KXAFA, KXAFA, KXAFA,
      KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA,
      KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA, KXAFA,
      KXAFA, KXAFA, KXAFA, KXAFA, KXX3, KXX3, KXX3, KXAFA, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
      KXX3, KXX3, KXX3, KXX3, KXX3, KXX3, KXX3,
};

inline bool
Class1::f2 (const char *map, const char **src, char **dst,
		    const char *end)
{
  if (g ((*src)[1]) && g ((*src)[2]))
    {
      char c = (static_cast < unsigned char >((*src)[1])) & 0xf;
      if (map[c] == KXAFA)
	{
	}
      else if (f1 (dst, end, c))
	{
	}
    }
  return true;
}

bool
Class1::f3 (const char *src, char *dst, const char *end)
{
  while (dst < end)
    {
      char c = *src;
      char m = p9t42[c];
      switch (m)
	{
	case KXYYZ:
	  *dst = '\0';
	case KXFI9:
 	  if (!f2 (p9t42, &src, &dst, end))
	    ;
	}
    }
  return false;
}
