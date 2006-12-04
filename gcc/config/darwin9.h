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
    %{g*:%{!gstabs*:%{!gnone: dsymutil %{o*:%*}%{!o:a.out}}}}}}}}}}}}"
