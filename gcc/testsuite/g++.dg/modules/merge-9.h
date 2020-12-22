using size_t = __SIZE_TYPE__;

namespace std
{
// This is a builtin, but should not be a global tree
  enum class align_val_t: size_t {};
  // as is this
  class type_info;
}
