// PR c++/106740
// { dg-additional-options -Wno-non-template-friend }

template <typename> struct EnumClass { friend int toString(EnumClass); };
struct AmhsConvInfoCoFw {
  enum AftnTypeXMsgTypeEnum {};
  typedef EnumClass<AftnTypeXMsgTypeEnum> AftnTypeXMsgType;
  const int getAftnTypeXMsgTypeAsStr() const;
  struct MtcuAxgwInfo {
    AftnTypeXMsgType mAftnTypeXMsgType;
  };
};
const int AmhsConvInfoCoFw::getAftnTypeXMsgTypeAsStr() const {
  MtcuAxgwInfo __trans_tmp_1;
  toString(__trans_tmp_1.mAftnTypeXMsgType);
  return 0;
}
int toString(AmhsConvInfoCoFw::AftnTypeXMsgType);
