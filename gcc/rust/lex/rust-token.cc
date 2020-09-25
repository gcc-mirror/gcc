#include "rust-token.h"

#include "rust-diagnostics.h" // for error_at

namespace Rust {
// Hackily defined way to get token description for enum value using x-macros
const char *
get_token_description (TokenId id)
{
  switch (id)
    {
#define RS_TOKEN(name, descr)                                                  \
  case name:                                                                   \
    return descr;
#define RS_TOKEN_KEYWORD(x, y) RS_TOKEN (x, y)
      RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
    default:
      gcc_unreachable ();
    }
}

/* Hackily defined way to get token description as a string for enum value using
 * x-macros */
const char *
token_id_to_str (TokenId id)
{
  switch (id)
    {
#define RS_TOKEN(name, _)                                                      \
  case name:                                                                   \
    return #name;
#define RS_TOKEN_KEYWORD(x, y) RS_TOKEN (x, y)
      RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
    default:
      gcc_unreachable ();
    }
}

const char *
get_type_hint_string (PrimitiveCoreType type)
{
  switch (type)
    {
    case CORETYPE_BOOL:
      return "bool";
    case CORETYPE_CHAR:
      return "char";
    case CORETYPE_STR:
      return "str";
    // case CORETYPE_INT:
    case CORETYPE_ISIZE:
      return "isize";
    // case CORETYPE_UINT:
    case CORETYPE_USIZE:
      return "usize";
    case CORETYPE_F32:
      return "f32";
    case CORETYPE_F64:
      return "f64";
    case CORETYPE_I8:
      return "i8";
    case CORETYPE_I16:
      return "i16";
    case CORETYPE_I32:
      return "i32";
    case CORETYPE_I64:
      return "i64";
    case CORETYPE_I128:
      return "i128";
    case CORETYPE_U8:
      return "u8";
    case CORETYPE_U16:
      return "u16";
    case CORETYPE_U32:
      return "u32";
    case CORETYPE_U64:
      return "u64";
    case CORETYPE_U128:
      return "u128";
    case CORETYPE_UNKNOWN:
    default:
      return "unknown";
    }
}

const char *
Token::get_type_hint_str () const
{
  return get_type_hint_string (type_hint);
}

const std::string &
Token::get_str () const
{
  // FIXME: attempt to return null again
  // gcc_assert(str != NULL);

  // HACK: allow referencing an empty string
  static const std::string empty = "";

  if (str == NULL)
    {
      rust_error_at (get_locus (),
		     "attempted to get string for '%s', which has no string. "
		     "returning empty string instead.",
		     get_token_description ());
      return empty;
    }
  return *str;
}
} // namespace Rust
