#include <vector>
#include <string>
#include <set>
#include <stdio.h>
#include "riscv-opts.h"

struct version_t
{
  int major;
  int minor;
  version_t (int major, int minor,
	     enum riscv_isa_spec_class spec = ISA_SPEC_CLASS_NONE)
    : major (major), minor (minor)
  {}
  bool operator<(const version_t &other) const
  {
    if (major != other.major)
      return major < other.major;
    return minor < other.minor;
  }

  bool operator== (const version_t &other) const
  {
    return major == other.major && minor == other.minor;
  }
};

static void
print_ext_doc_entry (const std::string &ext_name, const std::string &full_name,
		     const std::string &desc,
		     const std::vector<version_t> &supported_versions)
{
  // Implementation of the function to print the documentation entry
  // for the extension.
  std::set<version_t> unique_versions;
  for (const auto &version : supported_versions)
    unique_versions.insert (version);
  printf ("@item %s\n", ext_name.c_str ());
  printf ("@tab");
  for (const auto &version : unique_versions)
    {
      printf (" %d.%d", version.major, version.minor);
    }
  printf ("\n");
  printf ("@tab %s", full_name.c_str ());
  if (desc.size ())
    printf (", %s", desc.c_str ());
  printf ("\n\n");
}

int
main ()
{
  puts ("; Target options for the RISC-V port of the compiler");
  puts (";");
  puts ("; Copyright (C) 2025 Free Software Foundation, Inc.");
  puts (";");
  puts ("; This file is part of GCC.");
  puts (";");
  puts (
    "; GCC is free software; you can redistribute it and/or modify it under");
  puts (
    "; the terms of the GNU General Public License as published by the Free");
  puts (
    "; Software Foundation; either version 3, or (at your option) any later");
  puts ("; version.");
  puts (";");
  puts ("; GCC is distributed in the hope that it will be useful, but WITHOUT");
  puts ("; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY");
  puts ("; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public");
  puts ("; License for more details.");
  puts (";");
  puts ("; You should have received a copy of the GNU General Public License");
  puts ("; along with GCC; see the file COPYING3.  If not see ");
  puts ("; <http://www.gnu.org/licenses/>.");

  puts ("; This file is generated automatically using");
  puts (";  gcc/config/riscv/gen-riscv-ext-opt.cc from:");
  puts (";       gcc/config/riscv/riscv-ext.def");
  puts ("");
  puts ("; Please *DO NOT* edit manually.");

  std::set<std::string> all_vars;
#define DEFINE_RISCV_EXT(NAME, UPPERCAE_NAME, FULL_NAME, DESC, URL, DEP_EXTS,  \
			 SUPPORTED_VERSIONS, FLAG_GROUP, BITMASK_GROUP_ID,     \
			 BITMASK_BIT_POSITION, EXTRA_EXTENSION_FLAGS)          \
  all_vars.insert ("riscv_" #FLAG_GROUP "_subext");
#include "riscv-ext.def"
#undef DEFINE_RISCV_EXT

  for (auto var : all_vars)
    {
      puts ("TargetVariable");
      printf ("int %s\n\n", var.c_str ());
    }

#define DEFINE_RISCV_EXT(NAME, UPPERCAE_NAME, FULL_NAME, DESC, URL, DEP_EXTS,  \
			 SUPPORTED_VERSIONS, FLAG_GROUP, BITMASK_GROUP_ID,     \
			 BITMASK_BIT_POSITION, EXTRA_EXTENSION_FLAGS)          \
  puts ("Mask(" #UPPERCAE_NAME ") Var(riscv_" #FLAG_GROUP "_subext)\n");
#include "riscv-ext.def"
#undef DEFINE_RISCV_EXT

  return 0;
}
