// { dg-lto-do link }

template <typename Ordinal>
struct DirectSerializationTraits
{
  static void fromCountToDirectBytes(const Ordinal count) {}
};
template<typename Ordinal> class SerializationTraits
  : public DirectSerializationTraits<Ordinal> { };
template <typename Ordinal>
class ConstValueTypeSerializationBuffer
{
public:
    ConstValueTypeSerializationBuffer(const Ordinal count)
      {
	typedef SerializationTraits<Ordinal> SerT;
	SerT::fromCountToDirectBytes(count);
      }
};
int main ()
{
  ConstValueTypeSerializationBuffer<int> charSendBuffer(1);
}
