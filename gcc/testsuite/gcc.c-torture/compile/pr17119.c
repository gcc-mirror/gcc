void func1 (void);
void func2 (void);

void
_mesa_DrawPixels (int width, int height, unsigned format,
		  unsigned type, const void * pixels)
{
  switch (format)
    {
    case 0x1900:
      func1 ();
      break;
    case 0x1907:
    case 0x80E0:
    case 0x1908:
    case 0x80E1:
    case 0x8000:
      func2 ();
      break;
    }
}

