/* Common VxWorks target definitions for GNU compiler.
   Copyright (C) 1999-2024 Free Software Foundation, Inc.
   Contributed by Wind River Systems.
   Rewritten by CodeSourcery, LLC.

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

/* ??? We use HAVE_INITFINI_ARRAY_SUPPORT in preprocessor guards in this
   header, which is conveyed by auto-host.h despite being a target property.
   #include auto-host.h here would trigger lots of conflicts so we rely on
   compiler .c files doing this before target configuration headers.  */

/* Assert that we are targeting VxWorks.  */
#undef TARGET_VXWORKS
#define TARGET_VXWORKS 1

/* ??? Even though assigned to a HOST driver hook, this function
   operates for all vxworks targets regardless of the current host.
   We will get warnings at build time if the macro happens to be
   redefined one way or another for a host.  */
struct cl_decoded_option;
extern void vxworks_driver_init (unsigned int *, struct cl_decoded_option **);

#define GCC_DRIVER_HOST_INITIALIZATION \
        vxworks_driver_init (&decoded_options_count, &decoded_options)

/* In kernel mode, VxWorks provides all the libraries itself, as well as
   the functionality of startup files, etc.  In RTP mode, it behaves more
   like a traditional Unix, with more external files.  Most of our specs
   must be aware of the difference.  */

/* Help locate system headers, assuming $sysroot set to $VSB_DIR on vx7 and
   $WIND_BASE/target prior to that.  Specs allow tailoring for RTP vs kernel,
   and -idirafter allows putting system directories after GCC's own directories
   for standard headers such as <stddef.h> or fixed include.

   Regarding fixed includes, note the effect of sysroot_headers_suffix_spec:

   For the case of VxWorks prior to 7 below, we have:

     #define SYSROOT_HEADERS_SUFFIX_SPEC "%{mrtp:/usr/h;:/h}"

   This results in

     $build_sysroot/h     ---> $prefix/include-fixed
     $build_sysroot/usr/h ---> $prefix/include-fixed/mrtp for -mrtp

   This is very different from what we'd get without a headers_suffix,
   which would be:

     $build_sysroot     ---> $prefix/include-fixed/h
                                                  /usr/h

   From (say) #include <assert.h>, we would find the fixed version
   in the first case, not in the second.  */

/* Since we provide a default -isystem, expand -isystem on the command
   line early.  Then restrict the amount of references we add when compiling
   self-tests, as these may be run in contexts where the VxWorks environment
   isn't available.  */

#if TARGET_VXWORKS7

/* We arrange not rely on fixed includes for vx7 and the headers spread over
   common kernel/rtp directories in addition to specific ones for each mode.
   Setup sysroot_headers_suffix_spec to deal with kernel/rtp distinction.  */

#undef SYSROOT_HEADERS_SUFFIX_SPEC
#define SYSROOT_HEADERS_SUFFIX_SPEC "%{mrtp:/usr/h;:/krnl/h}"

#undef VXWORKS_ADDITIONAL_CPP_SPEC
#define VXWORKS_ADDITIONAL_CPP_SPEC                     \
 "%{!nostdinc:%{!fself-test=*:                          \
    %{isystem*}                                         \
    -idirafter %:getenv(VSB_DIR /h)  \
    -idirafter %:getenv(VSB_DIR /share/h)  \
    -idirafter =/system \
    -idirafter =/public \
  }}"

#else /* TARGET_VXWORKS7 */

/* Prior to vx7, rtp and kernel headers are fairly segregated and fixincludes
   is needed on each set of headers to cope with expectations of not so old
   libstdc++.  A perfect use case for sysroot_headers_suffix.  */

#undef SYSROOT_HEADERS_SUFFIX_SPEC
#define SYSROOT_HEADERS_SUFFIX_SPEC "%{mrtp:/usr/h;:/h}"

#undef VXWORKS_ADDITIONAL_CPP_SPEC
#define VXWORKS_ADDITIONAL_CPP_SPEC		\
 "%{!nostdinc:%{!fself-test=*:			\
    %{isystem*}					\
    -idirafter =/wrn/coreip \
  }}"

#endif

/* Our ports rely on gnu-user.h, which #defines _POSIX_SOURCE for
   C++ by default.  VxWorks doesn't provide 100% of what this implies
   (e.g. ::mkstemp), so, arrange to prevent that by falling back to
   the default CPP spec for C++ as well.  */
#undef CPLUSPLUS_CPP_SPEC

/* For VxWorks static rtps, the system provides libc_internal.a for a variety
   of purposes. Care is needed to include it appropriately.

   - In some configurations, libc_internal fills in possible references from
     the static libc that we don't wouldn't satisfy ourselves, say, with
     libgcc.  An example is the __aeabi_memcpy family of functions on arm,
     which have very specific ABI allowances.

   - OTOH, in some configurations the library provides typical libgcc
     services, for example register save/restore entry points on powerpc. We
     want our libgcc to prevail for symbols it would provide, so place
     -lc_internal after -lc -lgcc.

   - libc_internal also contains __init/__fini functions for
     INITFINI_ARRAY support. However, the system expects these in
     every shared lib as well, with slightly different names, and it is
     simpler for us to provide our own versions through vxcrtstuff.

   In addition, some versions of VxWorks rely on explicit extra libraries for
   system calls and the set of base network libraries of common use varies
   across architectures.  The default settings defined here might be redefined
   by target specific port configuration files.  */

#define VXWORKS_SYSCALL_LIBS_RTP

#if TARGET_VXWORKS7
#define VXWORKS_NET_LIBS_RTP "-l%:if-exists-then-else(%:getenv(VSB_DIR /usr/h/public/rtnetStackLib.h) rtnet net)"
#else
#define VXWORKS_NET_LIBS_RTP "-lnet -ldsi"
#endif

#define VXWORKS_BASE_LIBS_RTP "-lc -lgcc %{!shared:-lc_internal}"

#define VXWORKS_EXTRA_LIBS_RTP

#define VXWORKS_LIBS_RTP \
  VXWORKS_SYSCALL_LIBS_RTP " " VXWORKS_NET_LIBS_RTP " " \
  VXWORKS_BASE_LIBS_RTP " " VXWORKS_EXTRA_LIBS_RTP

/* TLS configuration.  VxWorks 7 now always has proper TLS support.
   Earlier versions did not, not even for RTPS.  */
#define VXWORKS_HAVE_TLS TARGET_VXWORKS7

/* On Vx6 and previous, the libraries to pick up depends on the architecture,
   so cannot be defined for all archs at once.  On Vx7, a VSB is always needed
   and its structure is fixed and does not depend on the arch.  We can thus
   tell gcc where to look for when linking with RTP libraries.  Use
   STARTFILE_PREFIX_SPEC for this, instead of explicit -L options in LIB_SPEC,
   so they survive -nodefaultlibs.  */

/* On Vx7 RTP, we need to drag the __tls__ symbol to trigger initialization of
   tlsLib, responsible for TLS support by the OS.  */

#if TARGET_VXWORKS7

/* For static links, /usr/lib/common has everything. For dynamic links,
   /usr/lib/common/PIC has the static libs and objects that might be needed
   in the closure (e.g. crt0.o), while the shared version of standard deps
   (e.g. libc.so) are still in /usr/lib/common.  */
#undef  STARTFILE_PREFIX_SPEC
#define STARTFILE_PREFIX_SPEC \
  "%{shared|non-static:/usr/lib/common/PIC} /usr/lib/common"

#define TLS_SYM "-u __tls__"

#else

#define TLS_SYM ""

#endif

#undef VXWORKS_LIB_SPEC
#define	VXWORKS_LIB_SPEC						   \
"%{mrtp:%{!shared:%{non-static:-u " USER_LABEL_PREFIX "_STI__6__rtld -ldl} \
		  " TLS_SYM "                                              \
		  --start-group " VXWORKS_LIBS_RTP " --end-group}}"

#if TARGET_VXWORKS7
#define VXWORKS_EXTRA_LINK_SPEC ""
#else
/* Older VxWorks RTPs can only link with shared libs, and
   need special switches --force-dynamic --export-dynamic. */
#define VXWORKS_EXTRA_LINK_SPEC				\
"%{mrtp:%{!shared:%{non-static:--force-dynamic --export-dynamic}}}"
#endif

/* A default link_os expansion for RTPs, that cpu ports may override.  */
#undef VXWORKS_LINK_OS_SPEC
#define VXWORKS_LINK_OS_SPEC "%(link_os)"

/* The -B and -X switches are for DIAB based linking. */
#undef VXWORKS_BASE_LINK_SPEC
#define VXWORKS_BASE_LINK_SPEC				\
"%{!mrtp:-r}						\
 %{v:-V}						\
 %{shared:-shared}					\
 %{Bstatic:-Bstatic}					\
 %{Bdynamic:-Bdynamic}					\
 %{!Xbind-lazy:-z now}					\
 %{Xbind-now:%{Xbind-lazy:				\
   %e-Xbind-now and -Xbind-lazy are incompatible}}	\
 %{mrtp:-q %{!shared:%{!non-static:-static}}            \
        %{h*} %{R*} %{!T*: %(link_start)}"              \
        VXWORKS_LINK_OS_SPEC "}"

#undef VXWORKS_LINK_SPEC
#define VXWORKS_LINK_SPEC VXWORKS_BASE_LINK_SPEC " " VXWORKS_EXTRA_LINK_SPEC

/* Control how to include libgcc in the link closure, handling both "shared"
   and "non-static" in addition to "static-libgcc" when shared lib support is
   enabled.  */

#undef VXWORKS_LIBGCC_SPEC

/* libgcc_eh control; libgcc_eh.a is available either together with libgcc_s
   (mrtp and mcmodel!=large when configured with --enable-shared) or when the
   compiler is specially setup to support dual sjlj/table-based eh.  */

/* VX_LGCC_EH_SO1: The "-lgcc_eh" part we need in situations where we know a
   shared libgcc is available (ENABLE_SHARED_LIBGCC + mrtp multilib).  */

#define VX_LGCC_EH_SO1 " -lgcc_eh -lgcc"
/* Extra -lgcc to handle functions from libgcc_eh that refer to symbols
   exposed by libgcc and not guaranteed to be dragged in before -lgcc_eh
   appears.  */

/* VX_LGCC_EH_SO0: The "-lgcc_eh" part we need in situations where we know a
   shared libgcc is not available (!ENABLE_SHARED_LIBGCC or !mrtp multlib).  */

#if !defined(CONFIG_DUAL_EXCEPTIONS)

/* No shared lib && !DUAL_EH -> no libgcc_eh available at all.  */
#define VX_LGCC_EH_SO0

#else /* CONFIG_DUAL_EXCEPTIONS  */

/* No shared lib but DUAL_EH -> libgcc_eh around and spec handled by the driver
   depending on ENABLE_SHARED_LIBGCC.  If defined, the driver expects a regular
   sequence.  Otherwise, the driver is expected to turn -lgcc into -lgcc_eh on
   its own and just add an instance to address possible cross refs.  */

#if defined(ENABLE_SHARED_LIBGCC)
#define VX_LGCC_EH_SO0 " -lgcc_eh -lgcc"
#else
#define VX_LGCC_EH_SO0 " -lgcc"
#endif

#endif /* CONFIG_DUAL_EXCEPTIONS  */

#if defined(ENABLE_SHARED_LIBGCC)
#define VXWORKS_LIBGCC_SPEC                                             \
  "%{!mrtp|mcmodel=large:-lgcc" VX_LGCC_EH_SO0 ";"			\
  " :%{!static-libgcc:%{shared|non-static:-lgcc_s;:-lgcc" VX_LGCC_EH_SO1 "}} \
     %{static-libgcc:-lgcc" VX_LGCC_EH_SO1 "}}"
#else
#define VXWORKS_LIBGCC_SPEC "-lgcc" VX_LGCC_EH_SO0
#endif

/* Setup the crtstuff begin/end we might need for dwarf EH registration
   and/or INITFINI_ARRAY support.  */
#if (HAVE_INITFINI_ARRAY_SUPPORT					\
     || (DWARF2_UNWIND_INFO && !defined(CONFIG_SJLJ_EXCEPTIONS)))
#define VX_CRTBEGIN_SPEC "%{!shared:vx_crtbegin.o%s;:vx_crtbeginS.o%s}"
#define VX_CRTEND_SPEC   "%{!shared:vx_crtend.o%s;:vx_crtendS.o%s}"
#else
#define VX_CRTBEGIN_SPEC ""
#define VX_CRTEND_SPEC ""
#endif

#undef VXWORKS_STARTFILE_SPEC
#define VXWORKS_STARTFILE_SPEC \
  VX_CRTBEGIN_SPEC " %{mrtp:%{!shared:-l:crt0.o}}"

#undef VXWORKS_ENDFILE_SPEC
#define VXWORKS_ENDFILE_SPEC VX_CRTEND_SPEC

#undef  VXWORKS_CC1_SPEC
#if TARGET_VXWORKS7
#define VXWORKS_CC1_SPEC \
  "%(cc1_cpu) %{!mrtp:%{!ftls-model=*:-ftls-model=local-exec}}"
#else
#define VXWORKS_CC1_SPEC ""
#endif

/* Do VxWorks-specific parts of TARGET_OPTION_OVERRIDE.  */
#undef VXWORKS_OVERRIDE_OPTIONS
#define VXWORKS_OVERRIDE_OPTIONS vxworks_override_options ()
extern void vxworks_override_options (void);

/* Whether the VxWorks variant and mode supports constructors/destructors
   placed in .ctors/.dtors section or if we should generate proxy functions
   for them, with special names which munch knows how to collect.  On most
   versions of VxWorks, only the RTP loader supports .ctors/.dtors sections,
   not the kernel module loader.  */
#define TARGET_VXWORKS_HAVE_CTORS_DTORS TARGET_VXWORKS_RTP

/* Support for prioritized ctors/dtors is in sync with the support for sections
   on the VxWorks front, and is assumed to be provided by whatever linker level
   glue is required if we were configured with --enable-initfini-array.  */
#define SUPPORTS_INIT_PRIORITY \
  (TARGET_VXWORKS_HAVE_CTORS_DTORS || HAVE_INITFINI_ARRAY_SUPPORT)

#if !HAVE_INITFINI_ARRAY_SUPPORT
/* VxWorks requires special handling of constructors and destructors.
   All VxWorks configurations must use these functions.  */
#undef TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR vxworks_asm_out_constructor
#undef TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR vxworks_asm_out_destructor
extern void vxworks_asm_out_constructor (rtx symbol, int priority);
extern void vxworks_asm_out_destructor (rtx symbol, int priority);
#endif

/* Override the vxworks-dummy.h definitions.  TARGET_VXWORKS_RTP
   is defined by vxworks.opt.  */
#undef VXWORKS_GOTT_BASE
#define VXWORKS_GOTT_BASE "__GOTT_BASE__"
#undef VXWORKS_GOTT_INDEX
#define VXWORKS_GOTT_INDEX "__GOTT_INDEX__"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_VXWORKS64 ? "long int" : "int")

#undef SIZE_TYPE
#define SIZE_TYPE (TARGET_VXWORKS64 ? "long unsigned int" : "unsigned int")

/* Assumptions on the target libc.  VxWorks 7, post SR600, provides a C11
   runtime without sincos support.  */
#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION \
  (TARGET_VXWORKS7 ? default_libc_has_function : no_c99_libc_has_function)

/* Both kernels and RTPs have the facilities required by this macro.  */
#define TARGET_POSIX_IO

/* A VxWorks implementation of TARGET_OS_CPP_BUILTINS.  */

/* The VxWorks personality we rely on, controlling which sections of system
   headers files we trigger.  This might be redefined on targets where the
   base VxWorks environment doesn't come with a GNU toolchain.  */

#define VXWORKS_PERSONALITY "gnu"

#define VXWORKS_OS_CPP_BUILTINS()					\
  do									\
    {									\
      builtin_define ("__vxworks");					\
      builtin_define ("__VXWORKS__");					\
      builtin_assert ("system=unix");					\
      if (TARGET_VXWORKS_RTP)						\
	builtin_define ("__RTP__");					\
      else								\
	builtin_define ("_WRS_KERNEL");					\
      builtin_define ("TOOL_FAMILY=" VXWORKS_PERSONALITY);		\
      builtin_define ("TOOL=" VXWORKS_PERSONALITY);			\
      if (TARGET_VXWORKS7)						\
        {								\
           builtin_define ("_VSB_CONFIG_FILE=<config/vsbConfig.h>");	\
           								\
	   /* _ALLOW_KEYWORD_MACROS is needed on VxWorks 7 to		\
	      prevent compilation failures triggered by our		\
	      definition of "inline" in ansidecl when "inline"		\
	      is not a keyword.  */					\
	   if (!flag_isoc99 && !c_dialect_cxx())			\
             builtin_define ("_ALLOW_KEYWORD_MACROS");			\
        }								\
      /* C++ support relies on C99 features from C++11, even C++98	\
         for listdc++ in particular, with corresponding checks at	\
         configure time.  Make sure C99 features are exposed by the	\
         system headers.  */						\
      if (c_dialect_cxx())						\
        builtin_define("_C99");						\
    }									\
  while (0)

/* For specific CPU macro definitions expected by the system headers,
   different versions of VxWorks expect different forms of macros,
   such as "_VX_CPU=..." on Vx7 and some variants of Vx6, or "CPU=..."
   on all Vx6 and earlier.  Setup a common prefix macro here, that
   arch specific ports can reuse.  */

#if TARGET_VXWORKS7
#define VX_CPU_PREFIX "_VX_"
#else
#define VX_CPU_PREFIX ""
#endif

#define VXWORKS_KIND VXWORKS_KIND_NORMAL

/* The diab linker does not handle .gnu_attribute sections.  */
#undef HAVE_AS_GNU_ATTRIBUTE

/* We call vxworks's cacheTextUpdate instead of CLEAR_INSN_CACHE if
   needed.  We don't want to force a call on targets that don't define
   cache-clearing insns nor CLEAR_INSN_CACHE.  */
#undef TARGET_EMIT_CALL_BUILTIN___CLEAR_CACHE
#define TARGET_EMIT_CALL_BUILTIN___CLEAR_CACHE \
  vxworks_emit_call_builtin___clear_cache
extern void vxworks_emit_call_builtin___clear_cache (rtx begin, rtx end);

/* Default dwarf control values, accounting for non-gdb debuggers that come
   with VxWorks.  */

#undef DWARF_VERSION_DEFAULT
#define DWARF_VERSION_DEFAULT (TARGET_VXWORKS7 ? 3 : 2)

#undef DWARF_GNAT_ENCODINGS_DEFAULT
#define DWARF_GNAT_ENCODINGS_DEFAULT \
  (TARGET_VXWORKS7 ? DWARF_GNAT_ENCODINGS_MINIMAL : DWARF_GNAT_ENCODINGS_ALL)

/* The default configuration of incremental LTO linking (-flinker-output=rel)
   warns if an object file included in the link does not contain LTO bytecode,
   because in this case the output will not contain it either, thus preventing
   further incremental LTO linking.  We do not do repeated incremental linking
   so silence the warning (instead of passing -flinker-output=nolto-rel).  */
#undef LTO_PLUGIN_SPEC
#define LTO_PLUGIN_SPEC "%{!mrtp:-plugin-opt=-linker-output-auto-nolto-rel}"
