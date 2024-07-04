void abort (void);
void exit (int);

struct { long f8:8; long f24:24; } a;
struct { long f32:32; } b;

int
main (void)
{
  if (sizeof (a) != sizeof (b))
    abort ();
  exit (0);
}
