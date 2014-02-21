// PR c++/60274

typedef const char *const&  ProtocolIdType;

template <ProtocolIdType protocolId>
struct C {
  typedef int ProtocolVersion;
  struct D {
    ProtocolVersion GetProtocolVersion();
  };
};
template <ProtocolIdType protocolId>
typename C<protocolId>::ProtocolVersion C<protocolId>::D::GetProtocolVersion()
{
    return 1;
}
