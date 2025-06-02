#include <vector>
#include <string>
#include <set>
#include <stdio.h>
#include "riscv-opts.h"

struct version_t
{
  int major_version;
  int minor_version;
  version_t (int major, int minor,
	     enum riscv_isa_spec_class spec = ISA_SPEC_CLASS_NONE)
    : major_version (major), minor_version (minor)
  {}
  bool operator<(const version_t &other) const
  {
    if (major_version != other.major_version)
      return major_version < other.major_version;
    return minor_version < other.minor_version;
  }

  bool operator== (const version_t &other) const
  {
    return major_version == other.major_version && minor_version == other.minor_version;
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
      printf (" %d.%d", version.major_version, version.minor_version);
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
  puts ("@c Copyright (C) 2025 Free Software Foundation, Inc.");
  puts ("@c This is part of the GCC manual.");
  puts ("@c For copying conditions, see the file gcc/doc/include/fdl.texi.");
  puts ("");
  puts ("@c This file is generated automatically using");
  puts ("@c  gcc/config/riscv/gen-riscv-ext-texi.cc from:");
  puts ("@c       gcc/config/riscv/riscv-ext.def");
  puts ("@c       gcc/config/riscv/riscv-opts.h");
  puts ("");
  puts ("@c Please *DO NOT* edit manually.");
  puts ("");
  puts ("@multitable @columnfractions .10 .10 .80");
  puts ("@headitem Extension Name @tab Supported Version @tab Description");
  puts ("");

  /* g extension is a very speical extension that no clear version...  */
  puts ("@item g");
  puts ("@tab -");
  puts (
    "@tab General-purpose computing base extension, @samp{g} will expand to");
  puts ("@samp{i}, @samp{m}, @samp{a}, @samp{f}, @samp{d}, @samp{zicsr} and");
  puts ("@samp{zifencei}.");
  puts ("");

#define DEFINE_RISCV_EXT(NAME, UPPERCAE_NAME, FULL_NAME, DESC, URL, DEP_EXTS,  \
			 SUPPORTED_VERSIONS, FLAG_GROUP, BITMASK_GROUP_ID,     \
			 BITMASK_BIT_POSITION, EXTRA_EXTENSION_FLAGS)          \
  print_ext_doc_entry (#NAME, FULL_NAME, DESC,                                 \
		       std::vector<version_t> SUPPORTED_VERSIONS);
#include "riscv-ext.def"
#undef DEFINE_RISCV_EXT

  puts ("@end multitable");
  return 0;
}
