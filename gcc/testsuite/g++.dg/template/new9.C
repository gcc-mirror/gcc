// PR c++/46277

class ggRGBE {
public:
    ggRGBE();
};
template <class T> class ggIO
{
  void readbody(int);
  ggRGBE *scanline;
};
template <class T> void
ggIO<T>::readbody(int width)
{
  scanline = new ggRGBE[width];
}
