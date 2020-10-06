typedef unsigned char __uint8_t;
typedef __uint8_t uint8_t;
typedef struct plane_t {
  uint8_t *p_pixels;
  int i_lines;
  int i_pitch;
} plane_t;

typedef struct {
  plane_t p[5];
} picture_t;

#define N 4

void __attribute__((noipa))
picture_Clone(picture_t *picture, picture_t *res)
{
  for (int i = 0; i < N; i++) {
    res->p[i].p_pixels = picture->p[i].p_pixels;
    res->p[i].i_lines = picture->p[i].i_lines;
    res->p[i].i_pitch = picture->p[i].i_pitch;
  }
}

int
main()
{
  picture_t aaa, bbb;
  uint8_t pixels[10] = {1, 1, 1, 1, 1, 1, 1, 1};

  for (unsigned i = 0; i < N; i++)
    aaa.p[i].p_pixels = pixels;

  picture_Clone (&aaa, &bbb);

  uint8_t c = 0;
  for (unsigned i = 0; i < N; i++)
    c += bbb.p[i].p_pixels[0];

  if (c != N)
    __builtin_abort ();
  return 0;
}
