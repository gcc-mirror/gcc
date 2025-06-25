#include <string>
#include <vector>
#include <stdio.h>

int
main ()
{
  puts ("@c Copyright (C) 2025 Free Software Foundation, Inc.");
  puts ("@c This is part of the GCC manual.");
  puts ("@c For copying conditions, see the file gcc/doc/include/fdl.texi.");
  puts ("");
  puts ("@c This file is generated automatically using");
  puts ("@c  gcc/config/riscv/gen-riscv-mcpu-texi.cc from:");
  puts ("@c       gcc/config/riscv/riscv-cores.def");
  puts ("");
  puts ("@c Please *DO NOT* edit manually.");
  puts ("");
  puts ("@samp{Core Name}");
  puts ("");
  puts ("@opindex mcpu");
  puts ("@item -mcpu=@var{processor-string}");
  puts ("Use architecture of and optimize the output for the given processor, specified");
  puts ("by particular CPU name. Permissible values for this option are:");
  puts ("");
  puts ("");

  std::vector<std::string> coreNames;

#define RISCV_CORE(CORE_NAME, ARCH, MICRO_ARCH) \
  coreNames.push_back (CORE_NAME);
#include "riscv-cores.def"
#undef RISCV_CORE

  for (size_t i = 0; i < coreNames.size(); ++i) {
    if (i == coreNames.size() - 1) {
      printf("@samp{%s}.\n", coreNames[i].c_str());
    } else {
      printf("@samp{%s},\n\n", coreNames[i].c_str());
    }
  }

  return 0;
}
