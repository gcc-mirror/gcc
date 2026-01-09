/* Compiler-specific implementation of GCC extensions to diagnostic output.
   Copyright (C) 2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef TREE_DIAGNOSTIC_SINK_EXTENSIONS_H
#define TREE_DIAGNOSTIC_SINK_EXTENSIONS_H

#include "opts-diagnostic.h"

class compiler_extension_factory : public gcc_extension_factory
{
public:
  std::unique_ptr<diagnostics::sink::extension>
  make_cfg_extension (diagnostics::sink &sink) const final override;
};

#endif /* TREE_DIAGNOSTIC_SINK_EXTENSIONS_H */
