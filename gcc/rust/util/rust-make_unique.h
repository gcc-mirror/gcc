#ifndef RUST_MAKE_UNIQUE_H
#define RUST_MAKE_UNIQUE_H

#include <memory>

namespace Rust {

template <typename T, typename... Ts>
std::unique_ptr<T>
make_unique (Ts &&...params)
{
  return std::unique_ptr<T> (new T (std::forward<Ts> (params)...));
}

} // namespace Rust

#endif // RUST_MAKE_UNIQUE_H
