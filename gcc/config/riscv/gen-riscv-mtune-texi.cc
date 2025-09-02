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
  puts ("@c  gcc/config/riscv/gen-riscv-mtune-texi.cc from:");
  puts ("@c       gcc/config/riscv/riscv-cores.def");
  puts ("");
  puts ("@c Please *DO NOT* edit manually.");
  puts ("");
  puts ("@samp{Tune Name}");
  puts ("");
  puts ("@opindex mtune");
  puts ("@item -mtune=@var{processor-string}");
  puts ("Optimize the output for the given processor, specified by microarchitecture or");
  puts ("particular CPU name.  Permissible values for this option are:");
  puts ("");
  puts ("");

  std::vector<std::string> tuneNames;

#define RISCV_TUNE(TUNE_NAME, PIPELINE_MODEL, TUNE_INFO) \
  tuneNames.push_back (TUNE_NAME);
#include "riscv-cores.def"
#undef RISCV_TUNE

  for (size_t i = 0; i < tuneNames.size(); ++i) {
    printf("@samp{%s},\n\n", tuneNames[i].c_str());
  }

  puts ("and all valid options for @option{-mcpu=}.");

  return 0;
}
