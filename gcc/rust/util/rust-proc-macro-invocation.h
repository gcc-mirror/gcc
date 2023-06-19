#ifndef RUST_PROC_MACRO_INVOCATION_H
#define RUST_PROC_MACRO_INVOCATION_H

#include "rust-mapping-common.h"
#include "rust-location.h"

namespace Rust {

class ProcMacroInvocable
{
public:
  virtual NodeId get_node_id () const = 0;
  virtual const std::string as_string () const = 0;
  virtual Location get_locus () const = 0;

  virtual ~ProcMacroInvocable () {}
};

} // namespace Rust

#endif /* ! RUST_PROC_MACRO_INVOCATION_H*/
