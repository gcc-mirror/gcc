// PR c++/109658

template <typename OutputStream> void encode(OutputStream, int *) {
  struct ValueBaseVisitor {
    void visit() { encodeString(); }
    void encodeString() {}
  };
}
int encode_json;
void encode_out() { encode(encode_out, &encode_json); }
