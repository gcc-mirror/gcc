// { dg-do compile }

void
filter_scanlines (void *src_buffer, void *dst_buffer, int dst_pitch, int width)
{
  int x;
  unsigned short *src, *dst_a, *dst_b;

  src = (unsigned short *) src_buffer;
  dst_a = (unsigned short *) dst_buffer;
  dst_b = ((unsigned short *) dst_buffer) + (dst_pitch >> 1);

  for (x = 0; x < width; x++)
    {
      unsigned char gs, gh;
      gs = src[x];
      gh = gs + (gs >> 1);
      dst_a[x] = (gh << 5) | (gh);
      dst_b[x] = ((gs  - gh) << 5)  | (gs  - gh);
    }
}
