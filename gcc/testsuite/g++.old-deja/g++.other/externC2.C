// Build don't link:
// Origin: Boris Zentner <boris@m2b.de>

extern "C"
{
struct xx
{
  int x;
  xx();
};

xx::xx()
{
  x = 0;
}
}
