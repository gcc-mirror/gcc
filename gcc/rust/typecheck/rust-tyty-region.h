#ifndef RUST_TYTY_REGION_H
#define RUST_TYTY_REGION_H

namespace Rust {
namespace TyTy {

class Region
{
  enum Variant : uint8_t
  {
    UNRESOLVED,
    STATIC,
    EARLY_BOUND,
    LATE_BOUND,
    NAMED,
    ANONYMOUS,
    ERASED,
  };

  uint32_t index;
  uint16_t debruijn_index;
  Variant variant;

public:
  Region () : Region (UNRESOLVED) {}
  Region (const Region &other)
    : index (other.index), debruijn_index (other.debruijn_index),
      variant (other.variant)
  {}
  Region (Region &&other) noexcept
    : index (other.index), debruijn_index (other.debruijn_index),
      variant (other.variant)
  {}
  Region &operator= (const Region &other)
  {
    if (this == &other)
      return *this;
    index = other.index;
    debruijn_index = other.debruijn_index;
    variant = other.variant;
    return *this;
  }
  Region &operator= (Region &&other) noexcept
  {
    if (this == &other)
      return *this;
    index = other.index;
    debruijn_index = other.debruijn_index;
    variant = other.variant;
    return *this;
  }

  static Region make_static () { return Region (STATIC); }
  static Region make_early_bound (uint32_t index)
  {
    return Region (EARLY_BOUND, index);
  }
  static Region make_late_bound (uint32_t index, uint16_t debruijn_index)
  {
    return Region (LATE_BOUND, index, debruijn_index);
  }
  static Region make_named (uint32_t index) { return Region (NAMED, index); }
  static Region make_anonymous () { return Region (ANONYMOUS); }
  static Region make_erased () { return Region (ERASED); }

  size_t get_index () const { return index; }

  bool is_static () const { return variant == STATIC; }
  bool is_early_bound () const { return variant == EARLY_BOUND; }
  bool is_late_bound () const { return variant == LATE_BOUND; }
  bool is_named () const { return variant == NAMED; }
  bool is_anonymous () const { return variant == ANONYMOUS; }

  void shift_down () { debruijn_index++; }

  WARN_UNUSED_RESULT std::string as_string () const
  {
    switch (variant)
      {
      case UNRESOLVED:
	return "'unresolved";
      case STATIC:
	return "'static";
      case EARLY_BOUND:
	return "'early(" + std::to_string (index) + ")";
      case LATE_BOUND:
	return "'late(" + std::to_string (debruijn_index) + ", "
	       + std::to_string (index) + ")";
      case NAMED:
	return "'named(" + std::to_string (index) + "";
      case ANONYMOUS:
	return "'_";
      case ERASED:
	return "'erased";
      }

    rust_unreachable ();
  }

private:
  explicit Region (Variant variant, uint32_t index = 0,
		   uint16_t debruijn_index = 0)
    : index (index), debruijn_index (debruijn_index), variant (variant)
  {}
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY_REGION_H
