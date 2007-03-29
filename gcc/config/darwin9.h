/* Prefer DWARF2.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#define DARWIN_PREFER_DWARF

#undef LINK_COMMAND_SPEC
#define LINK_COMMAND_SPEC "\
%{!fdump=*:%{!fsyntax-only:%{!c:%{!M:%{!MM:%{!E:%{!S:\
    %(linker) %l %X %{d} %{s} %{t} %{Z} \
    %{A} %{e*} %{m} %{r} %{x} \
    %{o*}%{!o:-o a.out} \
    %{!A:%{!nostdlib:%{!nostartfiles:%S}}} \
    %{L*} %{fopenmp:%:include(libgomp.spec)%(link_gomp)}   \
    %(link_libgcc) %o %{fprofile-arcs|fprofile-generate|coverage:-lgcov} \
    %{!nostdlib:%{!nodefaultlibs:%(link_ssp) %G %L}} \
    %{!A:%{!nostdlib:%{!nostartfiles:%E}}} %{T*} %{F*} }}}}}}}\n\
%{!fdump=*:%{!fsyntax-only:%{!c:%{!M:%{!MM:%{!E:%{!S:\
    %{.c|.cc|.C|.cpp|.c++|.CPP|.m|.mm: \
    %{g*:%{!gstabs*:%{!g0: dsymutil %{o*:%*}%{!o:a.out}}}}}}}}}}}}"

/* The linker can generate branch islands.  */
#define DARWIN_LINKER_GENERATES_ISLANDS 1

#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
  do {									\
    unsigned HOST_WIDE_INT _new_size = (SIZE);				\
    fprintf ((FILE), ".comm ");						\
    assemble_name ((FILE), (NAME));					\
    if (_new_size == 0) _new_size = 1;					\
    fprintf ((FILE), ","HOST_WIDE_INT_PRINT_UNSIGNED",%u\n",		\
	     _new_size, floor_log2 ((ALIGN) / BITS_PER_UNIT));		\
  } while (0)
