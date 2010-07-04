// Origin: PR c++/43558
// { dg-do compile }

class Compressible;
template <class T, class EngineTag> class Engine;
template <class T>
class Engine<T, Compressible>
{
  public:
    typedef T Element_t;
      //Element_t read(int);
      T read(int);
};

template <class T>
T Engine<T, Compressible>::read(int)
{
}

Engine<int, Compressible> x;

