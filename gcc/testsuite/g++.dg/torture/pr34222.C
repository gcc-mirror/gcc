/* { dg-do compile } */

namespace std __attribute__ ((__visibility__ ("default"))) {
    template<class _CharT>     struct char_traits;
  }
typedef long int ptrdiff_t;
namespace std __attribute__ ((__visibility__ ("default"))) {
    typedef ptrdiff_t streamsize;
    template<typename _CharT, typename _Traits = char_traits<_CharT> >     class basic_ifstream;
    typedef basic_ifstream<char> ifstream;
    class ios_base   {
    };
  }
template<class T> class Vector4 {
   public:
      inline Vector4();
      inline Vector4(T, T, T, T);
      T x, y, z, w;
  };
template<class T> class Matrix4 {
   public:
      Matrix4(const Vector4<T>&, const Vector4<T>&,             const Vector4<T>&, const Vector4<T>&);
      Matrix4(const Matrix4<T>& m);
      Vector4<T> r[4];
  };
typedef Vector4<float> Vec4f;
typedef Matrix4<float> Mat4f;
template<class T> Vector4<T>::Vector4() : x(0), y(0), z(0), w(0) {
  }
template<class T> Vector4<T>::Vector4(T _x, T _y, T _z, T _w) :     x(_x), y(_y), z(_z), w(_w) {
  }
template<class T> Matrix4<T>::Matrix4(const Vector4<T>& v0,                                       const Vector4<T>& v1,                                       const Vector4<T>& v2,                                       const Vector4<T>& v3) {
  }
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename _CharT, typename _Traits>     class basic_ios : public ios_base     {
      };
    template<typename _CharT, typename _Traits>     class basic_istream : virtual public basic_ios<_CharT, _Traits>     {
      public:
        typedef _CharT char_type;
        typedef basic_istream<_CharT, _Traits> __istream_type;
        __istream_type&       read(char_type* __s, streamsize __n);
      };
    template<typename _CharT, typename _Traits>     class basic_ifstream : public basic_istream<_CharT, _Traits>     {
      };
  }
using namespace std;
static float readFloat(ifstream& in) {
      float f;
      in.read((char*) &f, sizeof(float));
  }
Mat4f readMeshMatrix(ifstream& in, int nBytes) {
      float m00 = readFloat(in);
      float m01 = readFloat(in);
      float m02 = readFloat(in);
      float m10 = readFloat(in);
      float m11 = readFloat(in);
      float m12 = readFloat(in);
      float m20 = readFloat(in);
      float m21 = readFloat(in);
      float m22 = readFloat(in);
      float m30 = readFloat(in);
      float m31 = readFloat(in);
      float m32 = readFloat(in);
      return Mat4f(Vec4f(m00, m01, m02, 0),                  Vec4f(m10, m11, m12, 0),                  Vec4f(m20, m21, m22, 0),                  Vec4f(m30, m31, m32, 1));
  }
