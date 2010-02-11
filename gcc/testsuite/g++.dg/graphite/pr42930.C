/* { dg-options "-O1 -floop-block" } */

typedef unsigned char byte;
typedef unsigned int uint;
typedef unsigned char uint8;
namespace Common {
class NonCopyable {
};
template<class In, class Out>
Out copy(In first, In last, Out dst) {
 while (first != last)
  *dst++ = *first++;
}
template<class T>
class Array {
 uint _size;
 T *_storage;
public:
 Array<T>& operator=(const Array<T> &array) {
  copy(array._storage, array._storage + _size, _storage);
 }
};
}
namespace Graphics {
struct PixelFormat {
 inline PixelFormat() {
 }
 inline PixelFormat(byte BytesPerPixel,
      byte RBits, byte GBits, byte BBits, byte ABits,
      byte RShift, byte GShift, byte BShift, byte AShift) {
 }
};
};
namespace Cine {
static const Graphics::PixelFormat kLowPalFormat(2, 3, 3, 3, 0, 8, 4, 0, 0);
class Palette {
public:
 struct Color {
  uint8 r, g, b;
 };
 Palette(const Graphics::PixelFormat format = Graphics::PixelFormat(), const uint numColors = 0);
 bool empty() const;
 bool isValid() const;
 Common::Array<Color> _colors;
};
class FWRenderer : public Common::NonCopyable {
 Cine::Palette _activePal;
 void drawCommand();
};
void FWRenderer::drawCommand() {
 if (!_activePal.isValid() || _activePal.empty()) {
  _activePal = Cine::Palette(kLowPalFormat, 16);
 }
}
}
