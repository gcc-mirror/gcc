// Copyright (C) 2020 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#ifndef RUST_HIR_INFERENCE_VAR
#define RUST_HIR_INFERENCE_VAR

#include "rust-system.h"
#include "rust-hir-full-decls.h"

namespace Rust {
namespace HIR {

class GeneralInferenceVariable;
class IntegralInferenceVariable;
class FloatInferenceVariable;
class InferenceVarVisitor
{
public:
  virtual void visit (GeneralInferenceVariable &v) {}
  virtual void visit (IntegralInferenceVariable &v) {}
  virtual void visit (FloatInferenceVariable &v) {}
};

// Base
class InferenceVariable
{
public:
  virtual ~InferenceVariable () {}

  virtual std::string as_string () const = 0;

  HIR::Type *get_type () { return resolved; }

  void set_type (HIR::Type *type) { resolved = type; }

  bool was_resolved () { return resolved != nullptr; }

  virtual void accept_vis (InferenceVarVisitor &vis) = 0;

protected:
  InferenceVariable () : resolved (nullptr) {}

  HIR::Type *resolved;
};

class GeneralInferenceVariable : public InferenceVariable
{
public:
  GeneralInferenceVariable () : InferenceVariable () {}

  void accept_vis (InferenceVarVisitor &vis) { vis.visit (*this); };

  std::string as_string () const override
  {
    if (resolved)
      return resolved->as_string ();

    return "[G::?T]";
  }
};

class IntegralInferenceVariable : public InferenceVariable
{
public:
  IntegralInferenceVariable () : InferenceVariable () {}

  void accept_vis (InferenceVarVisitor &vis) { vis.visit (*this); };

  std::string as_string () const override
  {
    if (resolved)
      return resolved->as_string ();

    return "[I::?T]";
  }
};

class FloatInferenceVariable : public InferenceVariable
{
public:
  FloatInferenceVariable () : InferenceVariable () {}

  void accept_vis (InferenceVarVisitor &vis) { vis.visit (*this); };

  std::string as_string () const override
  {
    if (resolved)
      return resolved->as_string ();

    return "[F::?T]";
  }
};

} // namespace HIR
} // namespace Rust

#endif // RUST_HIR_INFERENCE_VAR
