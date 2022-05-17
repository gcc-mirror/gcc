// PR c++/105491
// { dg-do compile { target c++11 } }

class Message {
  virtual int GetMetadata();
};
class ProtobufCFileOptions : Message {
public:
  constexpr ProtobufCFileOptions(int);
  bool no_generate_;
  bool const_strings_;
  bool use_oneof_field_name_;
  bool gen_pack_helpers_;
  bool gen_init_helpers_;
};
constexpr ProtobufCFileOptions::ProtobufCFileOptions(int)
    : no_generate_(), const_strings_(), use_oneof_field_name_(),
      gen_pack_helpers_(), gen_init_helpers_() {}
struct ProtobufCFileOptionsDefaultTypeInternal {
  constexpr ProtobufCFileOptionsDefaultTypeInternal() : _instance({}) {}
  union {
    ProtobufCFileOptions _instance;
  };
} __constinit _ProtobufCFileOptions_default_instance_;
