// Build don't link:

template <int Dim, class T, class EngineTag>
class Engine {};

struct Brick;
 
template<int Dim, class T = double , class EngineTag = Brick >
struct ConstArray {
  static const int dimensions = Engine<Dim, T, EngineTag>::dimensions;
};
