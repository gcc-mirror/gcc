..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _option-summary:

Option Summary
**************

Here is a summary of all the options, grouped by type.  Explanations are
in the following sections.

*Overall Options*

  See :ref:`overall-options`.

  :option:`-c`  :option:`-S`  :option:`-E`  :option:`-o` :samp:`{file}` |gol|
  :option:`-dumpbase` :samp:`{dumpbase}`  :option:`-dumpbase-ext` :samp:`{auxdropsuf}` |gol|
  :option:`-dumpdir` :samp:`{dumppfx}`  :option:`-x` :samp:`{language}`  |gol|
  :option:`-v`  :option:`-###`  :option:`--help`:samp:`[={class}[,...]]`  :option:`--target-help`  :option:`--version` |gol|
  :option:`-pass-exit-codes`  :option:`-pipe`  :option:`-specs=file`  :option:`-wrapper`  |gol|
  :samp:`@{file}`  :option:`-ffile-prefix-map=old=new`  |gol|
  :option:`-fplugin=file`  :option:`-fplugin-arg-name=arg`  |gol|
  :option:`-fdump-ada-spec[-slim]` :option:`-fada-spec-parent=unit`  :option:`-fdump-go-spec=file`

*C Language Options*

  See :ref:`c-dialect-options`.

  :option:`-ansi`  :option:`-std=standard`  :option:`-aux-info` :samp:`{filename}` |gol|
  :option:`-fno-asm`  |gol|
  :option:`-fno-builtin`  :option:`-fno-builtin-function`  :option:`-fcond-mismatch` |gol|
  :option:`-ffreestanding`  :option:`-fgimple`  :option:`-fgnu-tm`  :option:`-fgnu89-inline`  :option:`-fhosted` |gol|
  :option:`-flax-vector-conversions`  :option:`-fms-extensions` |gol|
  :option:`-foffload=arg`  :option:`-foffload-options=arg` |gol|
  :option:`-fopenacc`  :option:`-fopenacc-dim=geom` |gol|
  :option:`-fopenmp`  :option:`-fopenmp-simd` |gol|
  :option:`-fpermitted-flt-eval-methods=standard` |gol|
  :option:`-fplan9-extensions`  :option:`-fsigned-bitfields`  :option:`-funsigned-bitfields` |gol|
  :option:`-fsigned-char`  :option:`-funsigned-char` :option:`-fstrict-flex-arrays[=n]` |gol|
  :option:`-fsso-struct=endianness`

*C++ Language Options*

  See :ref:`c++-dialect-options`.

  :option:`-fabi-version=n`  :option:`-fno-access-control` |gol|
  :option:`-faligned-new=n`  :option:`-fargs-in-order=n`  :option:`-fchar8_t`  :option:`-fcheck-new` |gol|
  :option:`-fconstexpr-depth=n`  :option:`-fconstexpr-cache-depth=n` |gol|
  :option:`-fconstexpr-loop-limit=n`  :option:`-fconstexpr-ops-limit=n` |gol|
  :option:`-fno-elide-constructors` |gol|
  :option:`-fno-enforce-eh-specs` |gol|
  :option:`-fno-gnu-keywords` |gol|
  :option:`-fno-implicit-templates` |gol|
  :option:`-fno-implicit-inline-templates` |gol|
  :option:`-fno-implement-inlines`  |gol|
  :option:`-fmodule-header=kind` :option:`-fmodule-only` :option:`-fmodules-ts` |gol|
  :option:`-fmodule-implicit-inline` |gol|
  :option:`-fno-module-lazy` |gol|
  :option:`-fmodule-mapper=specification` |gol|
  :option:`-fmodule-version-ignore` |gol|
  :option:`-fms-extensions` |gol|
  :option:`-fnew-inheriting-ctors` |gol|
  :option:`-fnew-ttp-matching` |gol|
  :option:`-fno-nonansi-builtins`  :option:`-fnothrow-opt`  :option:`-fno-operator-names` |gol|
  :option:`-fno-optional-diags`  :option:`-fpermissive` |gol|
  :option:`-fno-pretty-templates` |gol|
  :option:`-fno-rtti`  :option:`-fsized-deallocation` |gol|
  :option:`-ftemplate-backtrace-limit=n` |gol|
  :option:`-ftemplate-depth=n` |gol|
  :option:`-fno-threadsafe-statics`  :option:`-fuse-cxa-atexit` |gol|
  :option:`-fno-weak`  :option:`-nostdinc++` |gol|
  :option:`-fvisibility-inlines-hidden` |gol|
  :option:`-fvisibility-ms-compat` |gol|
  :option:`-fext-numeric-literals` |gol|
  :option:`-flang-info-include-translate=header` |gol|
  :option:`-flang-info-include-translate-not` |gol|
  :option:`-flang-info-module-cmi=module`  |gol|
  :option:`-stdlib=libstdc++,libc++` |gol|
  :option:`-Wabi-tag`  :option:`-Wcatch-value`  :option:`-Wcatch-value=n` |gol|
  :option:`-Wno-class-conversion`  :option:`-Wclass-memaccess` |gol|
  :option:`-Wcomma-subscript`  :option:`-Wconditionally-supported` |gol|
  :option:`-Wno-conversion-null`  :option:`-Wctad-maybe-unsupported` |gol|
  :option:`-Wctor-dtor-privacy`  :option:`-Wdangling-reference` |gol|
  :option:`-Wno-delete-incomplete` |gol|
  :option:`-Wdelete-non-virtual-dtor`  :option:`-Wno-deprecated-array-compare` |gol|
  :option:`-Wdeprecated-copy` :option:`-Wdeprecated-copy-dtor` |gol|
  :option:`-Wno-deprecated-enum-enum-conversion` :option:`-Wno-deprecated-enum-float-conversion` |gol|
  :option:`-Weffc++`  :option:`-Wno-exceptions` :option:`-Wextra-semi`  :option:`-Wno-inaccessible-base` |gol|
  :option:`-Wno-inherited-variadic-ctor`  :option:`-Wno-init-list-lifetime` |gol|
  :option:`-Winvalid-imported-macros` |gol|
  :option:`-Wno-invalid-offsetof`  :option:`-Wno-literal-suffix` |gol|
  :option:`-Wmismatched-new-delete` :option:`-Wmismatched-tags` |gol|
  :option:`-Wmultiple-inheritance`  :option:`-Wnamespaces`  :option:`-Wnarrowing` |gol|
  :option:`-Wnoexcept`  :option:`-Wnoexcept-type`  :option:`-Wnon-virtual-dtor` |gol|
  :option:`-Wpessimizing-move`  :option:`-Wno-placement-new`  :option:`-Wplacement-new=n` |gol|
  :option:`-Wrange-loop-construct` :option:`-Wredundant-move` :option:`-Wredundant-tags` |gol|
  :option:`-Wreorder`  :option:`-Wregister` |gol|
  :option:`-Wstrict-null-sentinel`  :option:`-Wno-subobject-linkage`  :option:`-Wtemplates` |gol|
  :option:`-Wno-non-template-friend`  :option:`-Wold-style-cast` |gol|
  :option:`-Woverloaded-virtual`  :option:`-Wno-pmf-conversions` :option:`-Wself-move` :option:`-Wsign-promo` |gol|
  :option:`-Wsized-deallocation`  :option:`-Wsuggest-final-methods` |gol|
  :option:`-Wsuggest-final-types`  :option:`-Wsuggest-override`  |gol|
  :option:`-Wno-terminate`  :option:`-Wuseless-cast`  :option:`-Wno-vexing-parse`  |gol|
  :option:`-Wvirtual-inheritance`  |gol|
  :option:`-Wno-virtual-move-assign`  :option:`-Wvolatile`  :option:`-Wzero-as-null-pointer-constant`

*Objective-C and Objective-C++ Language Options*

  See :ref:`objective-c-and-objective-c++-dialect-options`.

  :option:`-fconstant-string-class=class-name` |gol|
  :option:`-fgnu-runtime`  :option:`-fnext-runtime` |gol|
  :option:`-fno-nil-receivers` |gol|
  :option:`-fobjc-abi-version=n` |gol|
  :option:`-fobjc-call-cxx-cdtors` |gol|
  :option:`-fobjc-direct-dispatch` |gol|
  :option:`-fobjc-exceptions` |gol|
  :option:`-fobjc-gc` |gol|
  :option:`-fobjc-nilcheck` |gol|
  :option:`-fobjc-std=objc1` |gol|
  :option:`-fno-local-ivars` |gol|
  :option:`-fivar-visibility=[public|protected|private|package]` |gol|
  :option:`-freplace-objc-classes` |gol|
  :option:`-fzero-link` |gol|
  :option:`-gen-decls` |gol|
  :option:`-Wassign-intercept`  :option:`-Wno-property-assign-default` |gol|
  :option:`-Wno-protocol` :option:`-Wobjc-root-class` :option:`-Wselector` |gol|
  :option:`-Wstrict-selector-match` |gol|
  :option:`-Wundeclared-selector`

*Diagnostic Message Formatting Options*

  See :ref:`diagnostic-message-formatting-options`.

  :option:`-fmessage-length=n`  |gol|
  :option:`-fdiagnostics-plain-output` |gol|
  :option:`-fdiagnostics-show-location=[once|every-line]` |gol|
  :option:`-fdiagnostics-color=[auto|never|always]` |gol|
  :option:`-fdiagnostics-urls=[auto|never|always]` |gol|
  :option:`-fdiagnostics-format=[text|sarif-stderr|sarif-file|json|json-stderr|json-file]`  |gol|
  :option:`-fno-diagnostics-show-option`  :option:`-fno-diagnostics-show-caret` |gol|
  :option:`-fno-diagnostics-show-labels`  :option:`-fno-diagnostics-show-line-numbers` |gol|
  :option:`-fno-diagnostics-show-cwe`  |gol|
  :option:`-fno-diagnostics-show-rule`  |gol|
  :option:`-fdiagnostics-minimum-margin-width=width` |gol|
  :option:`-fdiagnostics-parseable-fixits`  :option:`-fdiagnostics-generate-patch` |gol|
  :option:`-fdiagnostics-show-template-tree`  :option:`-fno-elide-type` |gol|
  :option:`-fdiagnostics-path-format=[none|separate-events|inline-events]` |gol|
  :option:`-fdiagnostics-show-path-depths` |gol|
  :option:`-fno-show-column` |gol|
  :option:`-fdiagnostics-column-unit=[display|byte]` |gol|
  :option:`-fdiagnostics-column-origin=origin` |gol|
  :option:`-fdiagnostics-escape-format=[unicode|bytes]`

*Warning Options*

  See :ref:`warning-options`.

  :option:`-fsyntax-only`  :option:`-fmax-errors=n`  :option:`-Wpedantic` |gol|
  :option:`-pedantic-errors` |gol|
  :option:`-w`  :option:`-Wextra`  :option:`-Wall`  :option:`-Wabi=n` |gol|
  :option:`-Waddress`  :option:`-Wno-address-of-packed-member`  :option:`-Waggregate-return` |gol|
  :option:`-Walloc-size-larger-than=byte-size`  :option:`-Walloc-zero` |gol|
  :option:`-Walloca`  :option:`-Walloca-larger-than=byte-size` |gol|
  :option:`-Wno-aggressive-loop-optimizations` |gol|
  :option:`-Warith-conversion` |gol|
  :option:`-Warray-bounds`  :option:`-Warray-bounds=n`  :option:`-Warray-compare` |gol|
  :option:`-Wno-attributes`  :option:`-Wattribute-alias=n` :option:`-Wno-attribute-alias` |gol|
  :option:`-Wno-attribute-warning`  |gol|
  :option:`-Wbidi-chars=[none|unpaired|any|ucn]` |gol|
  :option:`-Wbool-compare`  :option:`-Wbool-operation` |gol|
  :option:`-Wno-builtin-declaration-mismatch` |gol|
  :option:`-Wno-builtin-macro-redefined`  :option:`-Wc90-c99-compat`  :option:`-Wc99-c11-compat` |gol|
  :option:`-Wc11-c2x-compat` |gol|
  :option:`-Wc++-compat`  :option:`-Wc++11-compat`  :option:`-Wc++14-compat`  :option:`-Wc++17-compat`  |gol|
  :option:`-Wc++20-compat`   |gol|
  :option:`-Wno-c++11-extensions`  :option:`-Wno-c++14-extensions` :option:`-Wno-c++17-extensions`  |gol|
  :option:`-Wno-c++20-extensions`  :option:`-Wno-c++23-extensions`  |gol|
  :option:`-Wcast-align`  :option:`-Wcast-align=strict`  :option:`-Wcast-function-type`  :option:`-Wcast-qual`  |gol|
  :option:`-Wchar-subscripts` |gol|
  :option:`-Wclobbered`  :option:`-Wcomment` |gol|
  :option:`-Wconversion`  :option:`-Wno-coverage-mismatch`  :option:`-Wno-cpp` |gol|
  :option:`-Wdangling-else`  :option:`-Wdangling-pointer`  :option:`-Wdangling-pointer=n`  |gol|
  :option:`-Wdate-time` |gol|
  :option:`-Wno-deprecated`  :option:`-Wno-deprecated-declarations`  :option:`-Wno-designated-init` |gol|
  :option:`-Wdisabled-optimization` |gol|
  :option:`-Wno-discarded-array-qualifiers`  :option:`-Wno-discarded-qualifiers` |gol|
  :option:`-Wno-div-by-zero`  :option:`-Wdouble-promotion` |gol|
  :option:`-Wduplicated-branches`  :option:`-Wduplicated-cond` |gol|
  :option:`-Wempty-body`  :option:`-Wno-endif-labels`  :option:`-Wenum-compare`  :option:`-Wenum-conversion` |gol|
  :option:`-Wenum-int-mismatch` |gol|
  :option:`-Werror`  :option:`-Werror=*`  :option:`-Wexpansion-to-defined`  :option:`-Wfatal-errors` |gol|
  :option:`-Wfloat-conversion`  :option:`-Wfloat-equal`  :option:`-Wformat`  :option:`-Wformat=2` |gol|
  :option:`-Wno-format-contains-nul`  :option:`-Wno-format-extra-args`  |gol|
  :option:`-Wformat-nonliteral`  :option:`-Wformat-overflow=n` |gol|
  :option:`-Wformat-security`  :option:`-Wformat-signedness`  :option:`-Wformat-truncation=n` |gol|
  :option:`-Wformat-y2k`  :option:`-Wframe-address` |gol|
  :option:`-Wframe-larger-than=byte-size`  :option:`-Wno-free-nonheap-object` |gol|
  :option:`-Wno-if-not-aligned`  :option:`-Wno-ignored-attributes` |gol|
  :option:`-Wignored-qualifiers`  :option:`-Wno-incompatible-pointer-types` |gol|
  :option:`-Wimplicit`  :option:`-Wimplicit-fallthrough`  :option:`-Wimplicit-fallthrough=n` |gol|
  :option:`-Wno-implicit-function-declaration`  :option:`-Wno-implicit-int` |gol|
  :option:`-Winfinite-recursion` |gol|
  :option:`-Winit-self`  :option:`-Winline`  :option:`-Wno-int-conversion`  :option:`-Wint-in-bool-context` |gol|
  :option:`-Wno-int-to-pointer-cast`  :option:`-Wno-invalid-memory-model` |gol|
  :option:`-Winvalid-pch`  :option:`-Winvalid-utf8`  :option:`-Wno-unicode`  :option:`-Wjump-misses-init`  |gol|
  :option:`-Wlarger-than=byte-size`  :option:`-Wlogical-not-parentheses`  :option:`-Wlogical-op`  |gol|
  :option:`-Wlong-long`  :option:`-Wno-lto-type-mismatch` :option:`-Wmain`  :option:`-Wmaybe-uninitialized` |gol|
  :option:`-Wmemset-elt-size`  :option:`-Wmemset-transposed-args` |gol|
  :option:`-Wmisleading-indentation`  :option:`-Wmissing-attributes`  :option:`-Wmissing-braces` |gol|
  :option:`-Wmissing-field-initializers`  :option:`-Wmissing-format-attribute` |gol|
  :option:`-Wmissing-include-dirs`  :option:`-Wmissing-noreturn`  :option:`-Wno-missing-profile` |gol|
  :option:`-Wno-multichar`  :option:`-Wmultistatement-macros`  :option:`-Wnonnull`  :option:`-Wnonnull-compare` |gol|
  :option:`-Wnormalized=[none|id|nfc|nfkc]`  |gol|
  :option:`-Wnull-dereference`  :option:`-Wno-odr`  |gol|
  :option:`-Wopenacc-parallelism`  |gol|
  :option:`-Wopenmp-simd`  |gol|
  :option:`-Wno-overflow`  :option:`-Woverlength-strings`  :option:`-Wno-override-init-side-effects` |gol|
  :option:`-Wpacked`  :option:`-Wno-packed-bitfield-compat`  :option:`-Wpacked-not-aligned`  :option:`-Wpadded` |gol|
  :option:`-Wparentheses`  :option:`-Wno-pedantic-ms-format` |gol|
  :option:`-Wpointer-arith`  :option:`-Wno-pointer-compare`  :option:`-Wno-pointer-to-int-cast` |gol|
  :option:`-Wno-pragmas`  :option:`-Wno-prio-ctor-dtor`  :option:`-Wredundant-decls` |gol|
  :option:`-Wrestrict`  :option:`-Wno-return-local-addr`  :option:`-Wreturn-type` |gol|
  :option:`-Wno-scalar-storage-order`  :option:`-Wsequence-point` |gol|
  :option:`-Wshadow`  :option:`-Wshadow=global`  :option:`-Wshadow=local`  :option:`-Wshadow=compatible-local` |gol|
  :option:`-Wno-shadow-ivar` |gol|
  :option:`-Wno-shift-count-negative`  :option:`-Wno-shift-count-overflow`  :option:`-Wshift-negative-value` |gol|
  :option:`-Wno-shift-overflow`  :option:`-Wshift-overflow=n` |gol|
  :option:`-Wsign-compare`  :option:`-Wsign-conversion` |gol|
  :option:`-Wno-sizeof-array-argument` |gol|
  :option:`-Wsizeof-array-div` |gol|
  :option:`-Wsizeof-pointer-div`  :option:`-Wsizeof-pointer-memaccess` |gol|
  :option:`-Wstack-protector`  :option:`-Wstack-usage=byte-size`  :option:`-Wstrict-aliasing` |gol|
  :option:`-Wstrict-aliasing=n`  :option:`-Wstrict-overflow`  :option:`-Wstrict-overflow=n` |gol|
  :option:`-Wstring-compare` |gol|
  :option:`-Wno-stringop-overflow` :option:`-Wno-stringop-overread` |gol|
  :option:`-Wno-stringop-truncation` |gol|
  :option:`-Wsuggest-attribute=[pure|const|noreturn|format|malloc]`  |gol|
  :option:`-Wswitch`  :option:`-Wno-switch-bool`  :option:`-Wswitch-default`  :option:`-Wswitch-enum` |gol|
  :option:`-Wno-switch-outside-range`  :option:`-Wno-switch-unreachable`  :option:`-Wsync-nand` |gol|
  :option:`-Wsystem-headers`  :option:`-Wtautological-compare`  :option:`-Wtrampolines`  :option:`-Wtrigraphs` |gol|
  :option:`-Wtrivial-auto-var-init` :option:`-Wtsan` :option:`-Wtype-limits`  :option:`-Wundef` |gol|
  :option:`-Wuninitialized`  :option:`-Wunknown-pragmas` |gol|
  :option:`-Wunsuffixed-float-constants`  :option:`-Wunused` |gol|
  :option:`-Wunused-but-set-parameter`  :option:`-Wunused-but-set-variable` |gol|
  :option:`-Wunused-const-variable`  :option:`-Wunused-const-variable=n` |gol|
  :option:`-Wunused-function`  :option:`-Wunused-label`  :option:`-Wunused-local-typedefs` |gol|
  :option:`-Wunused-macros` |gol|
  :option:`-Wunused-parameter`  :option:`-Wno-unused-result` |gol|
  :option:`-Wunused-value`  :option:`-Wunused-variable` |gol|
  :option:`-Wno-varargs`  :option:`-Wvariadic-macros` |gol|
  :option:`-Wvector-operation-performance` |gol|
  :option:`-Wvla`  :option:`-Wvla-larger-than=byte-size`  :option:`-Wno-vla-larger-than` |gol|
  :option:`-Wvolatile-register-var`  :option:`-Wwrite-strings` |gol|
  :option:`-Wxor-used-as-pow` |gol|
  :option:`-Wzero-length-bounds`

*Static Analyzer Options*

  :option:`-fanalyzer` |gol|
  :option:`-fanalyzer-call-summaries` |gol|
  :option:`-fanalyzer-checker=name` |gol|
  :option:`-fno-analyzer-feasibility` |gol|
  :option:`-fanalyzer-fine-grained` |gol|
  :option:`-fno-analyzer-state-merge` |gol|
  :option:`-fno-analyzer-state-purge` |gol|
  :option:`-fanalyzer-transitivity` |gol|
  :option:`-fno-analyzer-undo-inlining` |gol|
  :option:`-fanalyzer-verbose-edges` |gol|
  :option:`-fanalyzer-verbose-state-changes` |gol|
  :option:`-fanalyzer-verbosity=level` |gol|
  :option:`-fdump-analyzer` |gol|
  :option:`-fdump-analyzer-callgraph` |gol|
  :option:`-fdump-analyzer-exploded-graph` |gol|
  :option:`-fdump-analyzer-exploded-nodes` |gol|
  :option:`-fdump-analyzer-exploded-nodes-2` |gol|
  :option:`-fdump-analyzer-exploded-nodes-3` |gol|
  :option:`-fdump-analyzer-exploded-paths` |gol|
  :option:`-fdump-analyzer-feasibility` |gol|
  :option:`-fdump-analyzer-json` |gol|
  :option:`-fdump-analyzer-state-purge` |gol|
  :option:`-fdump-analyzer-stderr` |gol|
  :option:`-fdump-analyzer-supergraph` |gol|
  :option:`-fdump-analyzer-untracked` |gol|
  :option:`-Wno-analyzer-double-fclose` |gol|
  :option:`-Wno-analyzer-double-free` |gol|
  :option:`-Wno-analyzer-exposure-through-output-file` |gol|
  :option:`-Wno-analyzer-exposure-through-uninit-copy` |gol|
  :option:`-Wno-analyzer-fd-access-mode-mismatch` |gol|
  :option:`-Wno-analyzer-fd-double-close` |gol|
  :option:`-Wno-analyzer-fd-leak` |gol|
  :option:`-Wno-analyzer-fd-use-after-close` |gol|
  :option:`-Wno-analyzer-fd-use-without-check` |gol|
  :option:`-Wno-analyzer-file-leak` |gol|
  :option:`-Wno-analyzer-free-of-non-heap` |gol|
  :option:`-Wno-analyzer-imprecise-fp-arithmetic` |gol|
  :option:`-Wno-analyzer-jump-through-null` |gol|
  :option:`-Wno-analyzer-malloc-leak` |gol|
  :option:`-Wno-analyzer-mismatching-deallocation` |gol|
  :option:`-Wno-analyzer-null-argument` |gol|
  :option:`-Wno-analyzer-null-dereference` |gol|
  :option:`-Wno-analyzer-out-of-bounds` |gol|
  :option:`-Wno-analyzer-possible-null-argument` |gol|
  :option:`-Wno-analyzer-possible-null-dereference` |gol|
  :option:`-Wno-analyzer-putenv-of-auto-var` |gol|
  :option:`-Wno-analyzer-shift-count-negative` |gol|
  :option:`-Wno-analyzer-shift-count-overflow` |gol|
  :option:`-Wno-analyzer-stale-setjmp-buffer` |gol|
  :option:`-Wno-analyzer-tainted-allocation-size` |gol|
  :option:`-Wno-analyzer-tainted-array-index` |gol|
  :option:`-Wno-analyzer-tainted-divisor` |gol|
  :option:`-Wno-analyzer-tainted-offset` |gol|
  :option:`-Wno-analyzer-tainted-size` |gol|
  :option:`-Wanalyzer-too-complex` |gol|
  :option:`-Wno-analyzer-unsafe-call-within-signal-handler` |gol|
  :option:`-Wno-analyzer-use-after-free` |gol|
  :option:`-Wno-analyzer-use-of-pointer-in-stale-stack-frame` |gol|
  :option:`-Wno-analyzer-use-of-uninitialized-value` |gol|
  :option:`-Wno-analyzer-va-arg-type-mismatch` |gol|
  :option:`-Wno-analyzer-va-list-exhausted` |gol|
  :option:`-Wno-analyzer-va-list-leak` |gol|
  :option:`-Wno-analyzer-va-list-use-after-va-end` |gol|
  :option:`-Wno-analyzer-write-to-const` |gol|
  :option:`-Wno-analyzer-write-to-string-literal`

*C and Objective-C-only Warning Options*

  :option:`-Wbad-function-cast`  :option:`-Wmissing-declarations` |gol|
  :option:`-Wmissing-parameter-type`  :option:`-Wmissing-prototypes`  :option:`-Wnested-externs` |gol|
  :option:`-Wold-style-declaration`  :option:`-Wold-style-definition` |gol|
  :option:`-Wstrict-prototypes`  :option:`-Wtraditional`  :option:`-Wtraditional-conversion` |gol|
  :option:`-Wdeclaration-after-statement`  :option:`-Wpointer-sign`

*Debugging Options*

  See :ref:`debugging-options`.

  :option:`-g`  :option:`-glevel`  :option:`-gdwarf`  :option:`-gdwarf-version` |gol|
  :option:`-gbtf` :option:`-gctf`  :option:`-gctflevel` |gol|
  :option:`-ggdb`  :option:`-grecord-gcc-switches`  :option:`-gno-record-gcc-switches` |gol|
  :option:`-gstrict-dwarf`  :option:`-gno-strict-dwarf` |gol|
  :option:`-gas-loc-support`  :option:`-gno-as-loc-support` |gol|
  :option:`-gas-locview-support`  :option:`-gno-as-locview-support` |gol|
  :option:`-gcolumn-info`  :option:`-gno-column-info`  :option:`-gdwarf32`  :option:`-gdwarf64` |gol|
  :option:`-gstatement-frontiers`  :option:`-gno-statement-frontiers` |gol|
  :option:`-gvariable-location-views`  :option:`-gno-variable-location-views` |gol|
  :option:`-ginternal-reset-location-views`  :option:`-gno-internal-reset-location-views` |gol|
  :option:`-ginline-points`  :option:`-gno-inline-points` |gol|
  :option:`-gvms` :option:`-gz=type` |gol|
  :option:`-gsplit-dwarf`  :option:`-gdescribe-dies`  :option:`-gno-describe-dies` |gol|
  :option:`-fdebug-prefix-map=old=new` :option:`-fdebug-types-section` |gol|
  :option:`-fno-eliminate-unused-debug-types` |gol|
  :option:`-femit-struct-debug-baseonly`  :option:`-femit-struct-debug-reduced` |gol|
  :option:`-femit-struct-debug-detailed[=spec-list]` |gol|
  :option:`-fno-eliminate-unused-debug-symbols`  :option:`-femit-class-debug-always` |gol|
  :option:`-fno-merge-debug-strings`  :option:`-fno-dwarf2-cfi-asm` |gol|
  :option:`-fvar-tracking`  :option:`-fvar-tracking-assignments`

*Optimization Options*

  See :ref:`optimize-options`.

  :option:`-faggressive-loop-optimizations` |gol|
  :option:`-falign-functions[=n[m:[n2[:m2]]]]` |gol|
  :option:`-falign-jumps[=n[m:[n2[:m2]]]]` |gol|
  :option:`-falign-labels[=n[m:[n2[:m2]]]]` |gol|
  :option:`-falign-loops[=n[m:[n2[:m2]]]]` |gol|
  :option:`-fno-allocation-dce` :option:`-fallow-store-data-races` |gol|
  :option:`-fassociative-math`  :option:`-fauto-profile`  :option:`-fauto-profile[=path]` |gol|
  :option:`-fauto-inc-dec`  :option:`-fbranch-probabilities` |gol|
  :option:`-fcaller-saves` |gol|
  :option:`-fcombine-stack-adjustments`  :option:`-fconserve-stack` |gol|
  :option:`-fcompare-elim`  :option:`-fcprop-registers`  :option:`-fcrossjumping` |gol|
  :option:`-fcse-follow-jumps`  :option:`-fcse-skip-blocks`  :option:`-fcx-fortran-rules` |gol|
  :option:`-fcx-limited-range` |gol|
  :option:`-fdata-sections`  :option:`-fdce`  :option:`-fdelayed-branch` |gol|
  :option:`-fdelete-null-pointer-checks`  :option:`-fdevirtualize`  :option:`-fdevirtualize-speculatively` |gol|
  :option:`-fdevirtualize-at-ltrans`  :option:`-fdse` |gol|
  :option:`-fearly-inlining`  :option:`-fipa-sra`  :option:`-fexpensive-optimizations`  :option:`-ffat-lto-objects` |gol|
  :option:`-ffast-math`  :option:`-ffinite-math-only`  :option:`-ffloat-store`  :option:`-fexcess-precision=style` |gol|
  :option:`-ffinite-loops` |gol|
  :option:`-fforward-propagate`  :option:`-ffp-contract=style`  :option:`-ffunction-sections` |gol|
  :option:`-fgcse`  :option:`-fgcse-after-reload`  :option:`-fgcse-las`  :option:`-fgcse-lm`  :option:`-fgraphite-identity` |gol|
  :option:`-fgcse-sm`  :option:`-fhoist-adjacent-loads`  :option:`-fif-conversion` |gol|
  :option:`-fif-conversion2`  :option:`-findirect-inlining` |gol|
  :option:`-finline-functions`  :option:`-finline-functions-called-once`  :option:`-finline-limit=n` |gol|
  :option:`-finline-small-functions` :option:`-fipa-modref` :option:`-fipa-cp`  :option:`-fipa-cp-clone` |gol|
  :option:`-fipa-bit-cp`  :option:`-fipa-vrp`  :option:`-fipa-pta`  :option:`-fipa-profile`  :option:`-fipa-pure-const` |gol|
  :option:`-fipa-reference`  :option:`-fipa-reference-addressable` |gol|
  :option:`-fipa-stack-alignment`  :option:`-fipa-icf`  :option:`-fira-algorithm=algorithm` |gol|
  :option:`-flive-patching=level` |gol|
  :option:`-fira-region=region`  :option:`-fira-hoist-pressure` |gol|
  :option:`-fira-loop-pressure`  :option:`-fno-ira-share-save-slots` |gol|
  :option:`-fno-ira-share-spill-slots` |gol|
  :option:`-fisolate-erroneous-paths-dereference`  :option:`-fisolate-erroneous-paths-attribute` |gol|
  :option:`-fivopts`  :option:`-fkeep-inline-functions`  :option:`-fkeep-static-functions` |gol|
  :option:`-fkeep-static-consts`  :option:`-flimit-function-alignment`  :option:`-flive-range-shrinkage` |gol|
  :option:`-floop-block`  :option:`-floop-interchange`  :option:`-floop-strip-mine` |gol|
  :option:`-floop-unroll-and-jam`  :option:`-floop-nest-optimize` |gol|
  :option:`-floop-parallelize-all`  :option:`-flra-remat`  :option:`-flto`  :option:`-flto-compression-level` |gol|
  :option:`-flto-partition=alg`  :option:`-fmerge-all-constants` |gol|
  :option:`-fmerge-constants`  :option:`-fmodulo-sched`  :option:`-fmodulo-sched-allow-regmoves` |gol|
  :option:`-fmove-loop-invariants`  :option:`-fmove-loop-stores`  :option:`-fno-branch-count-reg` |gol|
  :option:`-fno-defer-pop`  :option:`-fno-fp-int-builtin-inexact`  :option:`-fno-function-cse` |gol|
  :option:`-fno-guess-branch-probability`  :option:`-fno-inline`  :option:`-fno-math-errno`  :option:`-fno-peephole` |gol|
  :option:`-fno-peephole2`  :option:`-fno-printf-return-value`  :option:`-fno-sched-interblock` |gol|
  :option:`-fno-sched-spec`  :option:`-fno-signed-zeros` |gol|
  :option:`-fno-toplevel-reorder`  :option:`-fno-trapping-math`  :option:`-fno-zero-initialized-in-bss` |gol|
  :option:`-fomit-frame-pointer`  :option:`-foptimize-sibling-calls` |gol|
  :option:`-fpartial-inlining`  :option:`-fpeel-loops`  :option:`-fpredictive-commoning` |gol|
  :option:`-fprefetch-loop-arrays` |gol|
  :option:`-fprofile-correction` |gol|
  :option:`-fprofile-use`  :option:`-fprofile-use=path` :option:`-fprofile-partial-training` |gol|
  :option:`-fprofile-values` :option:`-fprofile-reorder-functions` |gol|
  :option:`-freciprocal-math`  :option:`-free`  :option:`-frename-registers`  :option:`-freorder-blocks` |gol|
  :option:`-freorder-blocks-algorithm=algorithm` |gol|
  :option:`-freorder-blocks-and-partition`  :option:`-freorder-functions` |gol|
  :option:`-frerun-cse-after-loop`  :option:`-freschedule-modulo-scheduled-loops` |gol|
  :option:`-frounding-math`  :option:`-fsave-optimization-record` |gol|
  :option:`-fsched2-use-superblocks`  :option:`-fsched-pressure` |gol|
  :option:`-fsched-spec-load`  :option:`-fsched-spec-load-dangerous` |gol|
  :option:`-fsched-stalled-insns-dep[=n]`  :option:`-fsched-stalled-insns[=n]` |gol|
  :option:`-fsched-group-heuristic`  :option:`-fsched-critical-path-heuristic` |gol|
  :option:`-fsched-spec-insn-heuristic`  :option:`-fsched-rank-heuristic` |gol|
  :option:`-fsched-last-insn-heuristic`  :option:`-fsched-dep-count-heuristic` |gol|
  :option:`-fschedule-fusion` |gol|
  :option:`-fschedule-insns`  :option:`-fschedule-insns2`  :option:`-fsection-anchors` |gol|
  :option:`-fselective-scheduling`  :option:`-fselective-scheduling2` |gol|
  :option:`-fsel-sched-pipelining`  :option:`-fsel-sched-pipelining-outer-loops` |gol|
  :option:`-fsemantic-interposition`  :option:`-fshrink-wrap`  :option:`-fshrink-wrap-separate` |gol|
  :option:`-fsignaling-nans` |gol|
  :option:`-fsingle-precision-constant`  :option:`-fsplit-ivs-in-unroller`  :option:`-fsplit-loops` |gol|
  :option:`-fsplit-paths` |gol|
  :option:`-fsplit-wide-types`  :option:`-fsplit-wide-types-early`  :option:`-fssa-backprop`  :option:`-fssa-phiopt` |gol|
  :option:`-fstdarg-opt`  :option:`-fstore-merging`  :option:`-fstrict-aliasing` :option:`-fipa-strict-aliasing` |gol|
  :option:`-fthread-jumps`  :option:`-ftracer`  :option:`-ftree-bit-ccp` |gol|
  :option:`-ftree-builtin-call-dce`  :option:`-ftree-ccp`  :option:`-ftree-ch` |gol|
  :option:`-ftree-coalesce-vars`  :option:`-ftree-copy-prop`  :option:`-ftree-dce`  :option:`-ftree-dominator-opts` |gol|
  :option:`-ftree-dse`  :option:`-ftree-forwprop`  :option:`-ftree-fre`  :option:`-fcode-hoisting` |gol|
  :option:`-ftree-loop-if-convert`  :option:`-ftree-loop-im` |gol|
  :option:`-ftree-phiprop`  :option:`-ftree-loop-distribution`  :option:`-ftree-loop-distribute-patterns` |gol|
  :option:`-ftree-loop-ivcanon`  :option:`-ftree-loop-linear`  :option:`-ftree-loop-optimize` |gol|
  :option:`-ftree-loop-vectorize` |gol|
  :option:`-ftree-parallelize-loops=n`  :option:`-ftree-pre`  :option:`-ftree-partial-pre`  :option:`-ftree-pta` |gol|
  :option:`-ftree-reassoc`  :option:`-ftree-scev-cprop`  :option:`-ftree-sink`  :option:`-ftree-slsr`  :option:`-ftree-sra` |gol|
  :option:`-ftree-switch-conversion`  :option:`-ftree-tail-merge` |gol|
  :option:`-ftree-ter`  :option:`-ftree-vectorize`  :option:`-ftree-vrp`  :option:`-ftrivial-auto-var-init` |gol|
  :option:`-funconstrained-commons` :option:`-funit-at-a-time`  :option:`-funroll-all-loops` |gol|
  :option:`-funroll-loops` :option:`-funsafe-math-optimizations`  :option:`-funswitch-loops` |gol|
  :option:`-fipa-ra`  :option:`-fvariable-expansion-in-unroller`  :option:`-fvect-cost-model`  :option:`-fvpt` |gol|
  :option:`-fweb`  :option:`-fwhole-program`  :option:`-fwpa`  :option:`-fuse-linker-plugin` :option:`-fzero-call-used-regs` |gol|
  :option:`--param` :samp:`{name}={value}` |gol|
  :option:`-O`  :option:`-O0`  :option:`-O1`  :option:`-O2`  :option:`-O3`  :option:`-Os`  :option:`-Ofast`  :option:`-Og`  :option:`-Oz`

*Program Instrumentation Options*

  See :ref:`instrumentation-options`.

  :option:`-p`  :option:`-pg`  :option:`-fprofile-arcs`  :option:`--coverage`  :option:`-ftest-coverage` |gol|
  :option:`-fprofile-abs-path` |gol|
  :option:`-fprofile-dir=path`  :option:`-fprofile-generate`  :option:`-fprofile-generate=path` |gol|
  :option:`-fprofile-info-section`  :option:`-fprofile-info-section=name` |gol|
  :option:`-fprofile-note=path` :option:`-fprofile-prefix-path=path` |gol|
  :option:`-fprofile-update=method` :option:`-fprofile-filter-files=regex` |gol|
  :option:`-fprofile-exclude-files=regex` |gol|
  :option:`-fprofile-reproducible=[multithreaded|parallel-runs|serial]`  |gol|
  :option:`-fsanitize=style`  :option:`-fsanitize-recover`  :option:`-fsanitize-recover=style` |gol|
  :option:`-fsanitize-trap`   :option:`-fsanitize-trap=style`  |gol|
  :option:`-fasan-shadow-offset=number`  :option:`-fsanitize-sections=s1,s2,...` |gol|
  :option:`-fsanitize-undefined-trap-on-error`  :option:`-fbounds-check` |gol|
  :option:`-fcf-protection=[full|branch|return|none|check]` |gol|
  :option:`-fharden-compares` :option:`-fharden-conditional-branches` |gol|
  :option:`-fstack-protector`  :option:`-fstack-protector-all`  :option:`-fstack-protector-strong` |gol|
  :option:`-fstack-protector-explicit`  :option:`-fstack-check` |gol|
  :option:`-fstack-limit-register=reg`  :option:`-fstack-limit-symbol=sym` |gol|
  :option:`-fno-stack-limit`  :option:`-fsplit-stack` |gol|
  :option:`-fvtable-verify=[std|preinit|none]` |gol|
  :option:`-fvtv-counts`  :option:`-fvtv-debug` |gol|
  :option:`-finstrument-functions`  :option:`-finstrument-functions-once` |gol|
  :option:`-finstrument-functions-exclude-function-list=sym,sym,...` |gol|
  :option:`-finstrument-functions-exclude-file-list=file,file,...` |gol|
  :option:`-fprofile-prefix-map=old=new`

*Preprocessor Options*

  See :ref:`preprocessor-options`.

  :option:`-Aquestion=answer` |gol|
  :option:`-A-question[=answer]` |gol|
  :option:`-C`  :option:`-CC`  :option:`-Dmacro[=defn]` |gol|
  :option:`-dD`  :option:`-dI`  :option:`-dM`  :option:`-dN`  :option:`-dU` |gol|
  :option:`-fdebug-cpp`  :option:`-fdirectives-only`  :option:`-fdollars-in-identifiers`  |gol|
  :option:`-fexec-charset=charset`  :option:`-fextended-identifiers`  |gol|
  :option:`-finput-charset=charset`  :option:`-flarge-source-files`  |gol|
  :option:`-fmacro-prefix-map=old=new` :option:`-fmax-include-depth=depth` |gol|
  :option:`-fno-canonical-system-headers`  :option:`-fpch-deps`  :option:`-fpch-preprocess`  |gol|
  :option:`-fpreprocessed`  :option:`-ftabstop=width`  :option:`-ftrack-macro-expansion`  |gol|
  :option:`-fwide-exec-charset=charset`  :option:`-fworking-directory` |gol|
  :option:`-H`  :option:`-imacros` :samp:`{file}`  :option:`-include` :samp:`{file}` |gol|
  :option:`-M`  :option:`-MD`  :option:`-MF`  :option:`-MG`  :option:`-MM`  :option:`-MMD`  :option:`-MP`  :option:`-MQ`  :option:`-MT` :option:`-Mno-modules` |gol|
  :option:`-no-integrated-cpp`  :option:`-P`  :option:`-pthread`  :option:`-remap` |gol|
  :option:`-traditional`  :option:`-traditional-cpp`  :option:`-trigraphs` |gol|
  :option:`-Umacro`  :option:`-undef`  |gol|
  :option:`-Wp,option`  :option:`-Xpreprocessor` :samp:`{option}`

*Assembler Options*

  See :ref:`assembler-options`.

  :option:`-Wa,option`  :option:`-Xassembler` :samp:`{option}`

*Linker Options*

  See :ref:`link-options`.

  :samp:`{object-file-name}`  :option:`-fuse-ld=linker`  :option:`-llibrary` |gol|
  :option:`-nostartfiles`  :option:`-nodefaultlibs`  :option:`-nolibc`  :option:`-nostdlib`  :option:`-nostdlib++` |gol|
  :option:`-e` :samp:`{entry}`  :option:`--entry=entry` |gol|
  :option:`-pie`  :option:`-pthread`  :option:`-r`  :option:`-rdynamic` |gol|
  :option:`-s`  :option:`-static`  :option:`-static-pie`  :option:`-static-libgcc`  :option:`-static-libstdc++` |gol|
  :option:`-static-libasan`  :option:`-static-libtsan`  :option:`-static-liblsan`  :option:`-static-libubsan` |gol|
  :option:`-shared`  :option:`-shared-libgcc`  :option:`-symbolic` |gol|
  :option:`-T` :samp:`{script}`  :option:`-Wl,option`  :option:`-Xlinker` :samp:`{option}` |gol|
  :option:`-u` :samp:`{symbol}`  :option:`-z` :samp:`{keyword}`

*Directory Options*

  See :ref:`directory-options`.

  :option:`-Bprefix`  :option:`-Idir`  :option:`-I-` |gol|
  :option:`-idirafter` :samp:`{dir}` |gol|
  :option:`-imacros` :samp:`{file}`  :option:`-imultilib` :samp:`{dir}` |gol|
  :option:`-iplugindir=dir`  :option:`-iprefix` :samp:`{file}` |gol|
  :option:`-iquote` :samp:`{dir}`  :option:`-isysroot` :samp:`{dir}`  :option:`-isystem` :samp:`{dir}` |gol|
  :option:`-iwithprefix` :samp:`{dir}`  :option:`-iwithprefixbefore` :samp:`{dir}`  |gol|
  :option:`-Ldir`  :option:`-no-canonical-prefixes`  :option:`--no-sysroot-suffix` |gol|
  :option:`-nostdinc`  :option:`-nostdinc++`  :option:`--sysroot=dir`

*Code Generation Options*

  See :ref:`code-gen-options`.

  :option:`-fcall-saved-reg`  :option:`-fcall-used-reg` |gol|
  :option:`-ffixed-reg`  :option:`-fexceptions` |gol|
  :option:`-fnon-call-exceptions`  :option:`-fdelete-dead-exceptions`  :option:`-funwind-tables` |gol|
  :option:`-fasynchronous-unwind-tables` |gol|
  :option:`-fno-gnu-unique` |gol|
  :option:`-finhibit-size-directive`  :option:`-fcommon`  :option:`-fno-ident` |gol|
  :option:`-fpcc-struct-return`  :option:`-fpic`  :option:`-fPIC`  :option:`-fpie`  :option:`-fPIE`  :option:`-fno-plt` |gol|
  :option:`-fno-jump-tables` :option:`-fno-bit-tests` |gol|
  :option:`-frecord-gcc-switches` |gol|
  :option:`-freg-struct-return`  :option:`-fshort-enums`  :option:`-fshort-wchar` |gol|
  :option:`-fverbose-asm`  :option:`-fpack-struct[=n]` |gol|
  :option:`-fleading-underscore`  :option:`-ftls-model=model` |gol|
  :option:`-fstack-reuse=reuse_level` |gol|
  :option:`-ftrampolines`  :option:`-ftrapv`  :option:`-fwrapv` |gol|
  :option:`-fvisibility=[default|internal|hidden|protected]` |gol|
  :option:`-fstrict-volatile-bitfields`  :option:`-fsync-libcalls`

*Developer Options*

  See :ref:`developer-options`.

  :option:`-dletters`  :option:`-dumpspecs`  :option:`-dumpmachine`  :option:`-dumpversion` |gol|
  :option:`-dumpfullversion`  :option:`-fcallgraph-info[=su,da]` |gol|
  :option:`-fchecking`  :option:`-fchecking=n` |gol|
  :option:`-fdbg-cnt-list`   :option:`-fdbg-cnt=counter-value-list` |gol|
  :option:`-fdisable-ipa-pass_name` |gol|
  :option:`-fdisable-rtl-pass_name` |gol|
  :option:`-fdisable-rtl-pass-name=range-list` |gol|
  :option:`-fdisable-tree-pass_name` |gol|
  :option:`-fdisable-tree-pass-name=range-list` |gol|
  :option:`-fdump-debug`  :option:`-fdump-earlydebug` |gol|
  :option:`-fdump-noaddr`  :option:`-fdump-unnumbered`  :option:`-fdump-unnumbered-links` |gol|
  :option:`-fdump-final-insns[=file]` |gol|
  :option:`-fdump-ipa-all`  :option:`-fdump-ipa-cgraph`  :option:`-fdump-ipa-inline` |gol|
  :option:`-fdump-lang-all` |gol|
  :option:`-fdump-lang-switch` |gol|
  :option:`-fdump-lang-switch`:samp:`-{options}` |gol|
  :option:`-fdump-lang-switch`:samp:`-{options}={filename}` |gol|
  :option:`-fdump-passes` |gol|
  :option:`-fdump-rtl-pass`  :option:`-fdump-rtl-pass=filename` |gol|
  :option:`-fdump-statistics` |gol|
  :option:`-fdump-tree-all` |gol|
  :option:`-fdump-tree-switch` |gol|
  :option:`-fdump-tree-switch`:samp:`-{options}` |gol|
  :option:`-fdump-tree-switch`:samp:`-{options}={filename}` |gol|
  :option:`-fcompare-debug[=opts]`  :option:`-fcompare-debug-second` |gol|
  :option:`-fenable-kind`:samp:`-{pass}` |gol|
  :option:`-fenable-kind`:samp:`-{pass}={range-list}` |gol|
  :option:`-fira-verbose=n` |gol|
  :option:`-flto-report`  :option:`-flto-report-wpa`  :option:`-fmem-report-wpa` |gol|
  :option:`-fmem-report`  :option:`-fpre-ipa-mem-report`  :option:`-fpost-ipa-mem-report` |gol|
  :option:`-fopt-info`  :option:`-fopt-info-options[=file]` |gol|
  :option:`-fmultiflags`  :option:`-fprofile-report` |gol|
  :option:`-frandom-seed=string`  :option:`-fsched-verbose=n` |gol|
  :option:`-fsel-sched-verbose`  :option:`-fsel-sched-dump-cfg`  :option:`-fsel-sched-pipelining-verbose` |gol|
  :option:`-fstats`  :option:`-fstack-usage`  :option:`-ftime-report`  :option:`-ftime-report-details` |gol|
  :option:`-fvar-tracking-assignments-toggle`  :option:`-gtoggle` |gol|
  :option:`-print-file-name=library`  :option:`-print-libgcc-file-name` |gol|
  :option:`-print-multi-directory`  :option:`-print-multi-lib`  :option:`-print-multi-os-directory` |gol|
  :option:`-print-prog-name=program`  :option:`-print-search-dirs`  :option:`-Q` |gol|
  :option:`-print-sysroot`  :option:`-print-sysroot-headers-suffix` |gol|
  :option:`-save-temps`  :option:`-save-temps=cwd`  :option:`-save-temps=obj`  :option:`-time[=file]`

*Machine-Dependent Options*

  See :ref:`submodel-options`.

  .. This list is ordered alphanumerically by subsection name.
     Try and put the significant identifier (CPU or system) first,
     so users have a clue at guessing where the ones they want will be.

  *AArch64 Options*

  .. program:: AArch64

  :option:`-mabi=name`  :option:`-mbig-endian`  :option:`-mlittle-endian` |gol|
  :option:`-mgeneral-regs-only` |gol|
  :option:`-mcmodel=tiny`  :option:`-mcmodel=small`  :option:`-mcmodel=large` |gol|
  :option:`-mstrict-align`  :option:`-mno-strict-align` |gol|
  :option:`-momit-leaf-frame-pointer` |gol|
  :option:`-mtls-dialect=desc`  :option:`-mtls-dialect=traditional` |gol|
  :option:`-mtls-size=size` |gol|
  :option:`-mfix-cortex-a53-835769`  :option:`-mfix-cortex-a53-843419` |gol|
  :option:`-mlow-precision-recip-sqrt`  :option:`-mlow-precision-sqrt`  :option:`-mlow-precision-div` |gol|
  :option:`-mpc-relative-literal-loads` |gol|
  :option:`-msign-return-address=scope` |gol|
  :option:`-mbranch-protection=none|standard|pac-ret[+leaf+b-key|bti]` |gol|
  :option:`-mharden-sls=opts` |gol|
  :option:`-march=name`  :option:`-mcpu=name`  :option:`-mtune=name`  |gol|
  :option:`-moverride=string`  :option:`-mverbose-cost-dump` |gol|
  :option:`-mstack-protector-guard=guard` :option:`-mstack-protector-guard-reg=sysreg` |gol|
  :option:`-mstack-protector-guard-offset=offset` :option:`-mtrack-speculation` |gol|
  :option:`-moutline-atomics`

  *Adapteva Epiphany Options*

  .. program:: Adapteva Epiphany

  :option:`-mhalf-reg-file`  :option:`-mprefer-short-insn-regs` |gol|
  :option:`-mbranch-cost=num`  :option:`-mcmove`  :option:`-mnops=num`  :option:`-msoft-cmpsf` |gol|
  :option:`-msplit-lohi`  :option:`-mpost-inc`  :option:`-mpost-modify`  :option:`-mstack-offset=num` |gol|
  :option:`-mround-nearest`  :option:`-mlong-calls`  :option:`-mshort-calls`  :option:`-msmall16` |gol|
  :option:`-mfp-mode=mode`  :option:`-mvect-double`  :option:`-max-vect-align=num` |gol|
  :option:`-msplit-vecmove-early`  :option:`-m1reg-reg`

  *AMD GCN Options*

  .. program:: AMD GCN

  :option:`-march=gpu` :option:`-mtune=gpu` :option:`-mstack-size=bytes`

  *ARC Options*

  .. program:: ARC

  :option:`-mbarrel-shifter`  :option:`-mjli-always` |gol|
  :option:`-mcpu=cpu`  :option:`-mA6`  :option:`-mARC600`  :option:`-mA7`  :option:`-mARC700` |gol|
  :option:`-mdpfp`  :option:`-mdpfp-compact`  :option:`-mdpfp-fast`  :option:`-mno-dpfp-lrsr` |gol|
  :option:`-mea`  :option:`-mno-mpy`  :option:`-mmul32x16`  :option:`-mmul64`  :option:`-matomic` |gol|
  :option:`-mnorm`  :option:`-mspfp`  :option:`-mspfp-compact`  :option:`-mspfp-fast`  :option:`-msimd`  :option:`-msoft-float`  :option:`-mswap` |gol|
  :option:`-mcrc`  :option:`-mdsp-packa`  :option:`-mdvbf`  :option:`-mlock`  :option:`-mmac-d16`  :option:`-mmac-24`  :option:`-mrtsc`  :option:`-mswape` |gol|
  :option:`-mtelephony`  :option:`-mxy`  :option:`-misize`  :option:`-mannotate-align`  :option:`-marclinux`  :option:`-marclinux_prof` |gol|
  :option:`-mlong-calls`  :option:`-mmedium-calls`  :option:`-msdata`  :option:`-mirq-ctrl-saved` |gol|
  :option:`-mrgf-banked-regs`  :option:`-mlpc-width=width`  :option:`-G num` |gol|
  :option:`-mvolatile-cache`  :option:`-mtp-regno=regno` |gol|
  :option:`-malign-call`  :option:`-mauto-modify-reg`  :option:`-mbbit-peephole`  :option:`-mno-brcc` |gol|
  :option:`-mcase-vector-pcrel`  :option:`-mcompact-casesi`  :option:`-mno-cond-exec`  :option:`-mearly-cbranchsi` |gol|
  :option:`-mexpand-adddi`  :option:`-mindexed-loads`  :option:`-mlra`  :option:`-mlra-priority-none` |gol|
  :option:`-mlra-priority-compact` :option:`-mlra-priority-noncompact`  :option:`-mmillicode` |gol|
  :option:`-mmixed-code`  :option:`-mq-class`  :option:`-mRcq`  :option:`-mRcw`  :option:`-msize-level=level` |gol|
  :option:`-mtune=cpu`  :option:`-mmultcost=num`  :option:`-mcode-density-frame` |gol|
  :option:`-munalign-prob-threshold=probability`  :option:`-mmpy-option=multo` |gol|
  :option:`-mdiv-rem`  :option:`-mcode-density`  :option:`-mll64`  :option:`-mfpu=fpu`  :option:`-mrf16`  :option:`-mbranch-index`

  *ARM Options*

  .. program:: ARM

  :option:`-mapcs-frame`  :option:`-mno-apcs-frame` |gol|
  :option:`-mabi=name` |gol|
  :option:`-mapcs-stack-check`  :option:`-mno-apcs-stack-check` |gol|
  :option:`-mapcs-reentrant`  :option:`-mno-apcs-reentrant` |gol|
  :option:`-mgeneral-regs-only` |gol|
  :option:`-msched-prolog`  :option:`-mno-sched-prolog` |gol|
  :option:`-mlittle-endian`  :option:`-mbig-endian` |gol|
  :option:`-mbe8`  :option:`-mbe32` |gol|
  :option:`-mfloat-abi=name` |gol|
  :option:`-mfp16-format=name` |gol|
  :option:`-mthumb-interwork`  :option:`-mno-thumb-interwork` |gol|
  :option:`-mcpu=name`  :option:`-march=name`  :option:`-mfpu=name`  |gol|
  :option:`-mtune=name`  :option:`-mprint-tune-info` |gol|
  :option:`-mstructure-size-boundary=n` |gol|
  :option:`-mabort-on-noreturn` |gol|
  :option:`-mlong-calls`  :option:`-mno-long-calls` |gol|
  :option:`-msingle-pic-base`  :option:`-mno-single-pic-base` |gol|
  :option:`-mpic-register=reg` |gol|
  :option:`-mnop-fun-dllimport` |gol|
  :option:`-mpoke-function-name` |gol|
  :option:`-mthumb`  :option:`-marm`  :option:`-mflip-thumb` |gol|
  :option:`-mtpcs-frame`  :option:`-mtpcs-leaf-frame` |gol|
  :option:`-mcaller-super-interworking`  :option:`-mcallee-super-interworking` |gol|
  :option:`-mtp=name`  :option:`-mtls-dialect=dialect` |gol|
  :option:`-mword-relocations` |gol|
  :option:`-mfix-cortex-m3-ldrd` |gol|
  :option:`-mfix-cortex-a57-aes-1742098` |gol|
  :option:`-mfix-cortex-a72-aes-1655431` |gol|
  :option:`-munaligned-access` |gol|
  :option:`-mneon-for-64bits` |gol|
  :option:`-mslow-flash-data` |gol|
  :option:`-masm-syntax-unified` |gol|
  :option:`-mrestrict-it` |gol|
  :option:`-mverbose-cost-dump` |gol|
  :option:`-mpure-code` |gol|
  :option:`-mcmse` |gol|
  :option:`-mfix-cmse-cve-2021-35465` |gol|
  :option:`-mstack-protector-guard=guard` :option:`-mstack-protector-guard-offset=offset` |gol|
  :option:`-mfdpic`

  *AVR Options*

  .. program:: AVR

  :option:`-mmcu=mcu`  :option:`-mabsdata`  :option:`-maccumulate-args` |gol|
  :option:`-mbranch-cost=cost` |gol|
  :option:`-mcall-prologues`  :option:`-mgas-isr-prologues`  :option:`-mint8` |gol|
  :option:`-mdouble=bits` :option:`-mlong-double=bits` |gol|
  :option:`-mn_flash=size`  :option:`-mno-interrupts` |gol|
  :option:`-mmain-is-OS_task`  :option:`-mrelax`  :option:`-mrmw`  :option:`-mstrict-X`  :option:`-mtiny-stack` |gol|
  :option:`-mfract-convert-truncate` |gol|
  :option:`-mshort-calls`  :option:`-nodevicelib`  :option:`-nodevicespecs` |gol|
  :option:`-Waddr-space-convert`  :option:`-Wmisspelled-isr`

  *Blackfin Options*

  .. program:: Blackfin

  :option:`-mcpu=cpu[-sirevision]` |gol|
  :option:`-msim`  :option:`-momit-leaf-frame-pointer`  :option:`-mno-omit-leaf-frame-pointer` |gol|
  :option:`-mspecld-anomaly`  :option:`-mno-specld-anomaly`  :option:`-mcsync-anomaly`  :option:`-mno-csync-anomaly` |gol|
  :option:`-mlow-64k`  :option:`-mno-low64k`  :option:`-mstack-check-l1`  :option:`-mid-shared-library` |gol|
  :option:`-mno-id-shared-library`  :option:`-mshared-library-id=n` |gol|
  :option:`-mleaf-id-shared-library`  :option:`-mno-leaf-id-shared-library` |gol|
  :option:`-msep-data`  :option:`-mno-sep-data`  :option:`-mlong-calls`  :option:`-mno-long-calls` |gol|
  :option:`-mfast-fp`  :option:`-minline-plt`  :option:`-mmulticore`  :option:`-mcorea`  :option:`-mcoreb`  :option:`-msdram` |gol|
  :option:`-micplb`

  *C6X Options*

  .. program:: C6X

  :option:`-mbig-endian`  :option:`-mlittle-endian`  :option:`-march=cpu` |gol|
  :option:`-msim`  :option:`-msdata=sdata-type`

  *CRIS Options*

  .. program:: CRIS

  :option:`-mcpu=cpu`  :option:`-march=cpu` |gol|
  :option:`-mtune=cpu` :option:`-mmax-stack-frame=n` |gol|
  :option:`-metrax4`  :option:`-metrax100`  :option:`-mpdebug`  :option:`-mcc-init`  :option:`-mno-side-effects` |gol|
  :option:`-mstack-align`  :option:`-mdata-align`  :option:`-mconst-align` |gol|
  :option:`-m32-bit`  :option:`-m16-bit`  :option:`-m8-bit`  :option:`-mno-prologue-epilogue` |gol|
  :option:`-melf`  :option:`-maout`  :option:`-sim`  :option:`-sim2` |gol|
  :option:`-mmul-bug-workaround`  :option:`-mno-mul-bug-workaround`

  *C-SKY Options*

  .. program:: C-SKY

  :option:`-march=arch`  :option:`-mcpu=cpu` |gol|
  :option:`-mbig-endian`  :option:`-EB`  :option:`-mlittle-endian`  :option:`-EL` |gol|
  :option:`-mhard-float`  :option:`-msoft-float`  :option:`-mfpu=fpu`  :option:`-mdouble-float`  :option:`-mfdivdu` |gol|
  :option:`-mfloat-abi=name` |gol|
  :option:`-melrw`  :option:`-mistack`  :option:`-mmp`  :option:`-mcp`  :option:`-mcache`  :option:`-msecurity`  :option:`-mtrust` |gol|
  :option:`-mdsp`  :option:`-medsp`  :option:`-mvdsp` |gol|
  :option:`-mdiv`  :option:`-msmart`  :option:`-mhigh-registers`  :option:`-manchor` |gol|
  :option:`-mpushpop`  :option:`-mmultiple-stld`  :option:`-mconstpool`  :option:`-mstack-size`  :option:`-mccrt` |gol|
  :option:`-mbranch-cost=n`  :option:`-mcse-cc`  :option:`-msched-prolog` :option:`-msim`

  *Darwin Options*

  .. program:: Darwin

  :option:`-all_load`  :option:`-allowable_client`  :option:`-arch`  :option:`-arch_errors_fatal` |gol|
  :option:`-arch_only`  :option:`-bind_at_load`  :option:`-bundle`  :option:`-bundle_loader` |gol|
  :option:`-client_name`  :option:`-compatibility_version`  :option:`-current_version` |gol|
  :option:`-dead_strip` |gol|
  :option:`-dependency-file`  :option:`-dylib_file`  :option:`-dylinker_install_name` |gol|
  :option:`-dynamic`  :option:`-dynamiclib`  :option:`-exported_symbols_list` |gol|
  :option:`-filelist`  :option:`-flat_namespace`  :option:`-force_cpusubtype_ALL` |gol|
  :option:`-force_flat_namespace`  :option:`-headerpad_max_install_names` |gol|
  :option:`-iframework` |gol|
  :option:`-image_base`  :option:`-init`  :option:`-install_name`  :option:`-keep_private_externs` |gol|
  :option:`-multi_module`  :option:`-multiply_defined`  :option:`-multiply_defined_unused` |gol|
  :option:`-noall_load`   :option:`-no_dead_strip_inits_and_terms` |gol|
  :option:`-nofixprebinding`  :option:`-nomultidefs`  :option:`-noprebind`  :option:`-noseglinkedit` |gol|
  :option:`-pagezero_size`  :option:`-prebind`  :option:`-prebind_all_twolevel_modules` |gol|
  :option:`-private_bundle`  :option:`-read_only_relocs`  :option:`-sectalign` |gol|
  :option:`-sectobjectsymbols`  :option:`-whyload`  :option:`-seg1addr` |gol|
  :option:`-sectcreate`  :option:`-sectobjectsymbols`  :option:`-sectorder` |gol|
  :option:`-segaddr`  :option:`-segs_read_only_addr`  :option:`-segs_read_write_addr` |gol|
  :option:`-seg_addr_table`  :option:`-seg_addr_table_filename`  :option:`-seglinkedit` |gol|
  :option:`-segprot`  :option:`-segs_read_only_addr`  :option:`-segs_read_write_addr` |gol|
  :option:`-single_module`  :option:`-static`  :option:`-sub_library`  :option:`-sub_umbrella` |gol|
  :option:`-twolevel_namespace`  :option:`-umbrella`  :option:`-undefined` |gol|
  :option:`-unexported_symbols_list`  :option:`-weak_reference_mismatches` |gol|
  :option:`-whatsloaded`  :option:`-F`  :option:`-gused`  :option:`-gfull`  :option:`-mmacosx-version-min=version` |gol|
  :option:`-mkernel`  :option:`-mone-byte-bool`

  *DEC Alpha Options*

  .. program:: DEC Alpha

  :option:`-mno-fp-regs`  :option:`-msoft-float` |gol|
  :option:`-mieee`  :option:`-mieee-with-inexact`  :option:`-mieee-conformant` |gol|
  :option:`-mfp-trap-mode=mode`  :option:`-mfp-rounding-mode=mode` |gol|
  :option:`-mtrap-precision=mode`  :option:`-mbuild-constants` |gol|
  :option:`-mcpu=cpu-type`  :option:`-mtune=cpu-type` |gol|
  :option:`-mbwx`  :option:`-mmax`  :option:`-mfix`  :option:`-mcix` |gol|
  :option:`-mfloat-vax`  :option:`-mfloat-ieee` |gol|
  :option:`-mexplicit-relocs`  :option:`-msmall-data`  :option:`-mlarge-data` |gol|
  :option:`-msmall-text`  :option:`-mlarge-text` |gol|
  :option:`-mmemory-latency=time`

  *eBPF Options*

  .. program:: eBPF

  :option:`-mbig-endian` :option:`-mlittle-endian` :option:`-mkernel=version` |gol|
  :option:`-mframe-limit=bytes` :option:`-mxbpf` :option:`-mco-re` :option:`-mno-co-re` |gol|
  :option:`-mjmpext` :option:`-mjmp32` :option:`-malu32` :option:`-mcpu=version`

  *FR30 Options*

  .. program:: FR30

  :option:`-msmall-model`  :option:`-mno-lsim`

  *FT32 Options*

  .. program:: FT32

  :option:`-msim`  :option:`-mlra`  :option:`-mnodiv`  :option:`-mft32b`  :option:`-mcompress`  :option:`-mnopm`

  *FRV Options*

  .. program:: FRV

  :option:`-mgpr-32`  :option:`-mgpr-64`  :option:`-mfpr-32`  :option:`-mfpr-64` |gol|
  :option:`-mhard-float`  :option:`-msoft-float` |gol|
  :option:`-malloc-cc`  :option:`-mfixed-cc`  :option:`-mdword`  :option:`-mno-dword` |gol|
  :option:`-mdouble`  :option:`-mno-double` |gol|
  :option:`-mmedia`  :option:`-mno-media`  :option:`-mmuladd`  :option:`-mno-muladd` |gol|
  :option:`-mfdpic`  :option:`-minline-plt`  :option:`-mgprel-ro`  :option:`-multilib-library-pic` |gol|
  :option:`-mlinked-fp`  :option:`-mlong-calls`  :option:`-malign-labels` |gol|
  :option:`-mlibrary-pic`  :option:`-macc-4`  :option:`-macc-8` |gol|
  :option:`-mpack`  :option:`-mno-pack`  :option:`-mno-eflags`  :option:`-mcond-move`  :option:`-mno-cond-move` |gol|
  :option:`-moptimize-membar`  :option:`-mno-optimize-membar` |gol|
  :option:`-mscc`  :option:`-mno-scc`  :option:`-mcond-exec`  :option:`-mno-cond-exec` |gol|
  :option:`-mvliw-branch`  :option:`-mno-vliw-branch` |gol|
  :option:`-mmulti-cond-exec`  :option:`-mno-multi-cond-exec`  :option:`-mnested-cond-exec` |gol|
  :option:`-mno-nested-cond-exec`  :option:`-mtomcat-stats` |gol|
  :option:`-mTLS`  :option:`-mtls` |gol|
  :option:`-mcpu=cpu`

  *GNU/Linux Options*

  .. program:: GNU/Linux

  :option:`-mglibc`  :option:`-muclibc`  :option:`-mmusl`  :option:`-mbionic`  :option:`-mandroid` |gol|
  :option:`-tno-android-cc`  :option:`-tno-android-ld`

  *H8/300 Options*

  .. program:: H8/300

  :option:`-mrelax`  :option:`-mh`  :option:`-ms`  :option:`-mn`  :option:`-mexr`  :option:`-mno-exr`  :option:`-mint32`  :option:`-malign-300`

  *HPPA Options*

  .. program:: HPPA

  :option:`-march=architecture-type` |gol|
  :option:`-mcaller-copies`  :option:`-mdisable-fpregs`  :option:`-mdisable-indexing` |gol|
  :option:`-mfast-indirect-calls`  :option:`-mgas`  :option:`-mgnu-ld`   :option:`-mhp-ld` |gol|
  :option:`-mfixed-range=register-range` |gol|
  :option:`-mjump-in-delay`  :option:`-mlinker-opt`  :option:`-mlong-calls` |gol|
  :option:`-mlong-load-store`  :option:`-mno-disable-fpregs` |gol|
  :option:`-mno-disable-indexing`  :option:`-mno-fast-indirect-calls`  :option:`-mno-gas` |gol|
  :option:`-mno-jump-in-delay`  :option:`-mno-long-load-store` |gol|
  :option:`-mno-portable-runtime`  :option:`-mno-soft-float` |gol|
  :option:`-mno-space-regs`  :option:`-msoft-float`  :option:`-mpa-risc-1-0` |gol|
  :option:`-mpa-risc-1-1`  :option:`-mpa-risc-2-0`  :option:`-mportable-runtime` |gol|
  :option:`-mschedule=cpu-type`  :option:`-mspace-regs`  :option:`-msio`  :option:`-mwsio` |gol|
  :option:`-munix=unix-std`  :option:`-nolibdld`  :option:`-static`  :option:`-threads`

  *IA-64 Options*

  .. program:: IA-64

  :option:`-mbig-endian`  :option:`-mlittle-endian`  :option:`-mgnu-as`  :option:`-mgnu-ld`  :option:`-mno-pic` |gol|
  :option:`-mvolatile-asm-stop`  :option:`-mregister-names`  :option:`-msdata`  :option:`-mno-sdata` |gol|
  :option:`-mconstant-gp`  :option:`-mauto-pic`  :option:`-mfused-madd` |gol|
  :option:`-minline-float-divide-min-latency` |gol|
  :option:`-minline-float-divide-max-throughput` |gol|
  :option:`-mno-inline-float-divide` |gol|
  :option:`-minline-int-divide-min-latency` |gol|
  :option:`-minline-int-divide-max-throughput`  |gol|
  :option:`-mno-inline-int-divide` |gol|
  :option:`-minline-sqrt-min-latency`  :option:`-minline-sqrt-max-throughput` |gol|
  :option:`-mno-inline-sqrt` |gol|
  :option:`-mdwarf2-asm`  :option:`-mearly-stop-bits` |gol|
  :option:`-mfixed-range=register-range`  :option:`-mtls-size=tls-size` |gol|
  :option:`-mtune=cpu-type`  :option:`-milp32`  :option:`-mlp64` |gol|
  :option:`-msched-br-data-spec`  :option:`-msched-ar-data-spec`  :option:`-msched-control-spec` |gol|
  :option:`-msched-br-in-data-spec`  :option:`-msched-ar-in-data-spec`  :option:`-msched-in-control-spec` |gol|
  :option:`-msched-spec-ldc`  :option:`-msched-spec-control-ldc` |gol|
  :option:`-msched-prefer-non-data-spec-insns`  :option:`-msched-prefer-non-control-spec-insns` |gol|
  :option:`-msched-stop-bits-after-every-cycle`  :option:`-msched-count-spec-in-critical-path` |gol|
  :option:`-msel-sched-dont-check-control-spec`  :option:`-msched-fp-mem-deps-zero-cost` |gol|
  :option:`-msched-max-memory-insns-hard-limit`  :option:`-msched-max-memory-insns=max-insns`

  *LM32 Options*

  .. program:: LM32

  :option:`-mbarrel-shift-enabled`  :option:`-mdivide-enabled`  :option:`-mmultiply-enabled` |gol|
  :option:`-msign-extend-enabled`  :option:`-muser-enabled`

  *LoongArch Options*

  .. program:: LoongArch

  :option:`-march=cpu-type`  :option:`-mtune=cpu-type` :option:`-mabi=base-abi-type`
  :option:`-mfpu=fpu-type` :option:`-msoft-float` :option:`-msingle-float` :option:`-mdouble-float`
  :option:`-mbranch-cost=n`  :option:`-mcheck-zero-division` :option:`-mno-check-zero-division`
  :option:`-mcond-move-int`  :option:`-mno-cond-move-int`
  :option:`-mcond-move-float`  :option:`-mno-cond-move-float`
  :option:`-memcpy`  :option:`-mno-memcpy` :option:`-mstrict-align` :option:`-mno-strict-align`
  :option:`-mmax-inline-memcpy-size=n`
  :option:`-mexplicit-relocs` :option:`-mno-explicit-relocs`
  :option:`-mdirect-extern-access` :option:`-mno-direct-extern-access`
  :option:`-mcmodel=code-model`

  *M32R/D Options*

  .. program:: M32R/D

  :option:`-m32r2`  :option:`-m32rx`  :option:`-m32r` |gol|
  :option:`-mdebug` |gol|
  :option:`-malign-loops`  :option:`-mno-align-loops` |gol|
  :option:`-missue-rate=number` |gol|
  :option:`-mbranch-cost=number` |gol|
  :option:`-mmodel=code-size-model-type` |gol|
  :option:`-msdata=sdata-type` |gol|
  :option:`-mno-flush-func`  :option:`-mflush-func=name` |gol|
  :option:`-mno-flush-trap`  :option:`-mflush-trap=number` |gol|
  :option:`-G` :samp:`{num}`

  *M32C Options*

  .. program:: M32C

  :option:`-mcpu=cpu`  :option:`-msim`  :option:`-memregs=number`

  *M680x0 Options*

  .. program:: M680x0

  :option:`-march=arch`  :option:`-mcpu=cpu`  :option:`-mtune=tune` |gol|
  :option:`-m68000`  :option:`-m68020`  :option:`-m68020-40`  :option:`-m68020-60`  :option:`-m68030`  :option:`-m68040` |gol|
  :option:`-m68060`  :option:`-mcpu32`  :option:`-m5200`  :option:`-m5206e`  :option:`-m528x`  :option:`-m5307`  :option:`-m5407` |gol|
  :option:`-mcfv4e`  :option:`-mbitfield`  :option:`-mno-bitfield`  :option:`-mc68000`  :option:`-mc68020` |gol|
  :option:`-mnobitfield`  :option:`-mrtd`  :option:`-mno-rtd`  :option:`-mdiv`  :option:`-mno-div`  :option:`-mshort` |gol|
  :option:`-mno-short`  :option:`-mhard-float`  :option:`-m68881`  :option:`-msoft-float`  :option:`-mpcrel` |gol|
  :option:`-malign-int`  :option:`-mstrict-align`  :option:`-msep-data`  :option:`-mno-sep-data` |gol|
  :option:`-mshared-library-id=n`  :option:`-mid-shared-library`  :option:`-mno-id-shared-library` |gol|
  :option:`-mxgot`  :option:`-mno-xgot`  :option:`-mlong-jump-table-offsets`

  *MCore Options*

  .. program:: MCore

  :option:`-mhardlit`  :option:`-mno-hardlit`  :option:`-mdiv`  :option:`-mno-div`  :option:`-mrelax-immediates` |gol|
  :option:`-mno-relax-immediates`  :option:`-mwide-bitfields`  :option:`-mno-wide-bitfields` |gol|
  :option:`-m4byte-functions`  :option:`-mno-4byte-functions`  :option:`-mcallgraph-data` |gol|
  :option:`-mno-callgraph-data`  :option:`-mslow-bytes`  :option:`-mno-slow-bytes`  :option:`-mno-lsim` |gol|
  :option:`-mlittle-endian`  :option:`-mbig-endian`  :option:`-m210`  :option:`-m340`  :option:`-mstack-increment`

  *MeP Options*

  .. program:: MeP

  :option:`-mabsdiff`  :option:`-mall-opts`  :option:`-maverage`  :option:`-mbased=n`  :option:`-mbitops` |gol|
  :option:`-mc=n`  :option:`-mclip`  :option:`-mconfig=name`  :option:`-mcop`  :option:`-mcop32`  :option:`-mcop64`  :option:`-mivc2` |gol|
  :option:`-mdc`  :option:`-mdiv`  :option:`-meb`  :option:`-mel`  :option:`-mio-volatile`  :option:`-ml`  :option:`-mleadz`  :option:`-mm`  :option:`-mminmax` |gol|
  :option:`-mmult`  :option:`-mno-opts`  :option:`-mrepeat`  :option:`-ms`  :option:`-msatur`  :option:`-msdram`  :option:`-msim`  :option:`-msimnovec`  :option:`-mtf` |gol|
  :option:`-mtiny=n`

  *MicroBlaze Options*

  .. program:: MicroBlaze

  :option:`-msoft-float`  :option:`-mhard-float`  :option:`-msmall-divides`  :option:`-mcpu=cpu` |gol|
  :option:`-mmemcpy`  :option:`-mxl-soft-mul`  :option:`-mxl-soft-div`  :option:`-mxl-barrel-shift` |gol|
  :option:`-mxl-pattern-compare`  :option:`-mxl-stack-check`  :option:`-mxl-gp-opt`  :option:`-mno-clearbss` |gol|
  :option:`-mxl-multiply-high`  :option:`-mxl-float-convert`  :option:`-mxl-float-sqrt` |gol|
  :option:`-mbig-endian`  :option:`-mlittle-endian`  :option:`-mxl-reorder`  :option:`-mxl-mode-app-model` |gol|
  :option:`-mpic-data-is-text-relative`

  *MIPS Options*

  .. program:: MIPS

  :option:`-EL`  :option:`-EB`  :option:`-march=arch`  :option:`-mtune=arch` |gol|
  :option:`-mips1`  :option:`-mips2`  :option:`-mips3`  :option:`-mips4`  :option:`-mips32`  :option:`-mips32r2`  :option:`-mips32r3`  :option:`-mips32r5` |gol|
  :option:`-mips32r6`  :option:`-mips64`  :option:`-mips64r2`  :option:`-mips64r3`  :option:`-mips64r5`  :option:`-mips64r6` |gol|
  :option:`-mips16`  :option:`-mno-mips16`  :option:`-mflip-mips16` |gol|
  :option:`-minterlink-compressed`  :option:`-mno-interlink-compressed` |gol|
  :option:`-minterlink-mips16`  :option:`-mno-interlink-mips16` |gol|
  :option:`-mabi=abi`  :option:`-mabicalls`  :option:`-mno-abicalls` |gol|
  :option:`-mshared`  :option:`-mno-shared`  :option:`-mplt`  :option:`-mno-plt`  :option:`-mxgot`  :option:`-mno-xgot` |gol|
  :option:`-mgp32`  :option:`-mgp64`  :option:`-mfp32`  :option:`-mfpxx`  :option:`-mfp64`  :option:`-mhard-float`  :option:`-msoft-float` |gol|
  :option:`-mno-float`  :option:`-msingle-float`  :option:`-mdouble-float` |gol|
  :option:`-modd-spreg`  :option:`-mno-odd-spreg` |gol|
  :option:`-mabs=mode`  :option:`-mnan=encoding` |gol|
  :option:`-mdsp`  :option:`-mno-dsp`  :option:`-mdspr2`  :option:`-mno-dspr2` |gol|
  :option:`-mmcu`  :option:`-mmno-mcu` |gol|
  :option:`-meva`  :option:`-mno-eva` |gol|
  :option:`-mvirt`  :option:`-mno-virt` |gol|
  :option:`-mxpa`  :option:`-mno-xpa` |gol|
  :option:`-mcrc`  :option:`-mno-crc` |gol|
  :option:`-mginv`  :option:`-mno-ginv` |gol|
  :option:`-mmicromips`  :option:`-mno-micromips` |gol|
  :option:`-mmsa`  :option:`-mno-msa` |gol|
  :option:`-mloongson-mmi`  :option:`-mno-loongson-mmi` |gol|
  :option:`-mloongson-ext`  :option:`-mno-loongson-ext` |gol|
  :option:`-mloongson-ext2`  :option:`-mno-loongson-ext2` |gol|
  :option:`-mfpu=fpu-type` |gol|
  :option:`-msmartmips`  :option:`-mno-smartmips` |gol|
  :option:`-mpaired-single`  :option:`-mno-paired-single`  :option:`-mdmx`  :option:`-mno-mdmx` |gol|
  :option:`-mips3d`  :option:`-mno-mips3d`  :option:`-mmt`  :option:`-mno-mt`  :option:`-mllsc`  :option:`-mno-llsc` |gol|
  :option:`-mlong64`  :option:`-mlong32`  :option:`-msym32`  :option:`-mno-sym32` |gol|
  :option:`-Gnum`  :option:`-mlocal-sdata`  :option:`-mno-local-sdata` |gol|
  :option:`-mextern-sdata`  :option:`-mno-extern-sdata`  :option:`-mgpopt`  :option:`-mno-gopt` |gol|
  :option:`-membedded-data`  :option:`-mno-embedded-data` |gol|
  :option:`-muninit-const-in-rodata`  :option:`-mno-uninit-const-in-rodata` |gol|
  :option:`-mcode-readable=setting` |gol|
  :option:`-msplit-addresses`  :option:`-mno-split-addresses` |gol|
  :option:`-mexplicit-relocs`  :option:`-mno-explicit-relocs` |gol|
  :option:`-mcheck-zero-division`  :option:`-mno-check-zero-division` |gol|
  :option:`-mdivide-traps`  :option:`-mdivide-breaks` |gol|
  :option:`-mload-store-pairs`  :option:`-mno-load-store-pairs` |gol|
  :option:`-munaligned-access`  :option:`-mno-unaligned-access` |gol|
  :option:`-mmemcpy`  :option:`-mno-memcpy`  :option:`-mlong-calls`  :option:`-mno-long-calls` |gol|
  :option:`-mmad`  :option:`-mno-mad`  :option:`-mimadd`  :option:`-mno-imadd`  :option:`-mfused-madd`  :option:`-mno-fused-madd`  :option:`-nocpp` |gol|
  :option:`-mfix-24k`  :option:`-mno-fix-24k` |gol|
  :option:`-mfix-r4000`  :option:`-mno-fix-r4000`  :option:`-mfix-r4400`  :option:`-mno-fix-r4400` |gol|
  :option:`-mfix-r5900`  :option:`-mno-fix-r5900` |gol|
  :option:`-mfix-r10000`  :option:`-mno-fix-r10000`  :option:`-mfix-rm7000`  :option:`-mno-fix-rm7000` |gol|
  :option:`-mfix-vr4120`  :option:`-mno-fix-vr4120` |gol|
  :option:`-mfix-vr4130`  :option:`-mno-fix-vr4130`  :option:`-mfix-sb1`  :option:`-mno-fix-sb1` |gol|
  :option:`-mflush-func=func`  :option:`-mno-flush-func` |gol|
  :option:`-mbranch-cost=num`  :option:`-mbranch-likely`  :option:`-mno-branch-likely` |gol|
  :option:`-mcompact-branches=policy` |gol|
  :option:`-mfp-exceptions`  :option:`-mno-fp-exceptions` |gol|
  :option:`-mvr4130-align`  :option:`-mno-vr4130-align`  :option:`-msynci`  :option:`-mno-synci` |gol|
  :option:`-mlxc1-sxc1`  :option:`-mno-lxc1-sxc1`  :option:`-mmadd4`  :option:`-mno-madd4` |gol|
  :option:`-mrelax-pic-calls`  :option:`-mno-relax-pic-calls`  :option:`-mmcount-ra-address` |gol|
  :option:`-mframe-header-opt`  :option:`-mno-frame-header-opt`

  *MMIX Options*

  .. program:: MMIX

  :option:`-mlibfuncs`  :option:`-mno-libfuncs`  :option:`-mepsilon`  :option:`-mno-epsilon`  :option:`-mabi=gnu` |gol|
  :option:`-mabi=mmixware`  :option:`-mzero-extend`  :option:`-mknuthdiv`  :option:`-mtoplevel-symbols` |gol|
  :option:`-melf`  :option:`-mbranch-predict`  :option:`-mno-branch-predict`  :option:`-mbase-addresses` |gol|
  :option:`-mno-base-addresses`  :option:`-msingle-exit`  :option:`-mno-single-exit`

  *MN10300 Options*

  .. program:: MN10300

  :option:`-mmult-bug`  :option:`-mno-mult-bug` |gol|
  :option:`-mno-am33`  :option:`-mam33`  :option:`-mam33-2`  :option:`-mam34` |gol|
  :option:`-mtune=cpu-type` |gol|
  :option:`-mreturn-pointer-on-d0` |gol|
  :option:`-mno-crt0`  :option:`-mrelax`  :option:`-mliw`  :option:`-msetlb`

  *Moxie Options*

  .. program:: Moxie

  :option:`-meb`  :option:`-mel`  :option:`-mmul.x`  :option:`-mno-crt0`

  *MSP430 Options*

  .. program:: MSP430

  :option:`-msim`  :option:`-masm-hex`  :option:`-mmcu=`  :option:`-mcpu=`  :option:`-mlarge`  :option:`-msmall`  :option:`-mrelax` |gol|
  :option:`-mwarn-mcu` |gol|
  :option:`-mcode-region=`  :option:`-mdata-region=` |gol|
  :option:`-msilicon-errata=`  :option:`-msilicon-errata-warn=` |gol|
  :option:`-mhwmult=`  :option:`-minrt`  :option:`-mtiny-printf`  :option:`-mmax-inline-shift=`

  *NDS32 Options*

  .. program:: NDS32

  :option:`-mbig-endian`  :option:`-mlittle-endian` |gol|
  :option:`-mreduced-regs`  :option:`-mfull-regs` |gol|
  :option:`-mcmov`  :option:`-mno-cmov` |gol|
  :option:`-mext-perf`  :option:`-mno-ext-perf` |gol|
  :option:`-mext-perf2`  :option:`-mno-ext-perf2` |gol|
  :option:`-mext-string`  :option:`-mno-ext-string` |gol|
  :option:`-mv3push`  :option:`-mno-v3push` |gol|
  :option:`-m16bit`  :option:`-mno-16bit` |gol|
  :option:`-misr-vector-size=num` |gol|
  :option:`-mcache-block-size=num` |gol|
  :option:`-march=arch` |gol|
  :option:`-mcmodel=code-model` |gol|
  :option:`-mctor-dtor`  :option:`-mrelax`

  *Nios II Options*

  .. program:: Nios II

  :option:`-G` :samp:`{num}`  :option:`-mgpopt=option`  :option:`-mgpopt`  :option:`-mno-gpopt` |gol|
  :option:`-mgprel-sec=regexp`  :option:`-mr0rel-sec=regexp` |gol|
  :option:`-mel`  :option:`-meb` |gol|
  :option:`-mno-bypass-cache`  :option:`-mbypass-cache` |gol|
  :option:`-mno-cache-volatile`  :option:`-mcache-volatile` |gol|
  :option:`-mno-fast-sw-div`  :option:`-mfast-sw-div` |gol|
  :option:`-mhw-mul`  :option:`-mno-hw-mul`  :option:`-mhw-mulx`  :option:`-mno-hw-mulx`  :option:`-mno-hw-div`  :option:`-mhw-div` |gol|
  :option:`-mcustom-insn=N`  :option:`-mno-custom-insn` |gol|
  :option:`-mcustom-fpu-cfg=name` |gol|
  :option:`-mhal`  :option:`-msmallc`  :option:`-msys-crt0=name`  :option:`-msys-lib=name` |gol|
  :option:`-march=arch`  :option:`-mbmx`  :option:`-mno-bmx`  :option:`-mcdx`  :option:`-mno-cdx`

  *Nvidia PTX Options*

  .. program:: Nvidia PTX

  :option:`-m64`  :option:`-mmainkernel`  :option:`-moptimize`

  *OpenRISC Options*

  .. program:: OpenRISC

  :option:`-mboard=name`  :option:`-mnewlib`  :option:`-mhard-mul`  :option:`-mhard-div` |gol|
  :option:`-msoft-mul`  :option:`-msoft-div` |gol|
  :option:`-msoft-float`  :option:`-mhard-float`  :option:`-mdouble-float` :option:`-munordered-float` |gol|
  :option:`-mcmov`  :option:`-mror`  :option:`-mrori`  :option:`-msext`  :option:`-msfimm`  :option:`-mshftimm` |gol|
  :option:`-mcmodel=code-model`

  *PDP-11 Options*

  .. program:: PDP-11

  :option:`-mfpu`  :option:`-msoft-float`  :option:`-mac0`  :option:`-mno-ac0`  :option:`-m40`  :option:`-m45`  :option:`-m10` |gol|
  :option:`-mint32`  :option:`-mno-int16`  :option:`-mint16`  :option:`-mno-int32` |gol|
  :option:`-msplit`  :option:`-munix-asm`  :option:`-mdec-asm`  :option:`-mgnu-asm`  :option:`-mlra`

  *picoChip Options*

  .. program:: picoChip

  :option:`-mae=ae_type`  :option:`-mvliw-lookahead=N` |gol|
  :option:`-msymbol-as-address`  :option:`-mno-inefficient-warnings`

  *PowerPC Options*

  See :ref:`rs-6000-and-powerpc-options`.

  *PRU Options*

  .. program:: PRU

  :option:`-mmcu=mcu`  :option:`-minrt`  :option:`-mno-relax`  :option:`-mloop` |gol|
  :option:`-mabi=variant`

  *RISC-V Options*

  .. program:: RISC-V

  :option:`-mbranch-cost=N-instruction` |gol|
  :option:`-mplt`  :option:`-mno-plt` |gol|
  :option:`-mabi=ABI-string` |gol|
  :option:`-mfdiv`  :option:`-mno-fdiv` |gol|
  :option:`-mdiv`  :option:`-mno-div` |gol|
  :option:`-misa-spec=ISA-spec-string` |gol|
  :option:`-march=ISA-string` |gol|
  :option:`-mtune=processor-string` |gol|
  :option:`-mpreferred-stack-boundary=num` |gol|
  :option:`-msmall-data-limit=N-bytes` |gol|
  :option:`-msave-restore`  :option:`-mno-save-restore` |gol|
  :option:`-mshorten-memrefs`  :option:`-mno-shorten-memrefs` |gol|
  :option:`-mstrict-align`  :option:`-mno-strict-align` |gol|
  :option:`-mcmodel=medlow`  :option:`-mcmodel=medany` |gol|
  :option:`-mexplicit-relocs`  :option:`-mno-explicit-relocs` |gol|
  :option:`-mrelax`  :option:`-mno-relax` |gol|
  :option:`-mriscv-attribute`  :option:`-mno-riscv-attribute` |gol|
  :option:`-malign-data=type` |gol|
  :option:`-mbig-endian`  :option:`-mlittle-endian` |gol|
  :option:`-mstack-protector-guard=guard`  :option:`-mstack-protector-guard-reg=reg` |gol|
  :option:`-mstack-protector-guard-offset=offset`
  -mcsr-check -mno-csr-check

  .. program:: -mcsr-check -mno-csr-check

  *RL78 Options*

  .. program:: RL78

  :option:`-msim`  :option:`-mmul=none`  :option:`-mmul=g13`  :option:`-mmul=g14`  :option:`-mallregs` |gol|
  :option:`-mcpu=g10`  :option:`-mcpu=g13`  :option:`-mcpu=g14`  :option:`-mg10`  :option:`-mg13`  :option:`-mg14` |gol|
  :option:`-m64bit-doubles`  :option:`-m32bit-doubles`  :option:`-msave-mduc-in-interrupts`

  *RS/6000 and PowerPC Options*

  .. program:: IBM RS/6000 and PowerPC

  :option:`-mcpu=cpu-type` |gol|
  :option:`-mtune=cpu-type` |gol|
  :option:`-mcmodel=code-model` |gol|
  :option:`-mpowerpc64` |gol|
  :option:`-maltivec`  :option:`-mno-altivec` |gol|
  :option:`-mpowerpc-gpopt`  :option:`-mno-powerpc-gpopt` |gol|
  :option:`-mpowerpc-gfxopt`  :option:`-mno-powerpc-gfxopt` |gol|
  :option:`-mmfcrf`  :option:`-mno-mfcrf`  :option:`-mpopcntb`  :option:`-mno-popcntb`  :option:`-mpopcntd`  :option:`-mno-popcntd` |gol|
  :option:`-mfprnd`  :option:`-mno-fprnd` |gol|
  :option:`-mcmpb`  :option:`-mno-cmpb`  :option:`-mhard-dfp`  :option:`-mno-hard-dfp` |gol|
  :option:`-mfull-toc`   :option:`-mminimal-toc`  :option:`-mno-fp-in-toc`  :option:`-mno-sum-in-toc` |gol|
  :option:`-m64`  :option:`-m32`  :option:`-mxl-compat`  :option:`-mno-xl-compat`  :option:`-mpe` |gol|
  :option:`-malign-power`  :option:`-malign-natural` |gol|
  :option:`-msoft-float`  :option:`-mhard-float`  :option:`-mmultiple`  :option:`-mno-multiple` |gol|
  :option:`-mupdate`  :option:`-mno-update` |gol|
  :option:`-mavoid-indexed-addresses`  :option:`-mno-avoid-indexed-addresses` |gol|
  :option:`-mfused-madd`  :option:`-mno-fused-madd`  :option:`-mbit-align`  :option:`-mno-bit-align` |gol|
  :option:`-mstrict-align`  :option:`-mno-strict-align`  :option:`-mrelocatable` |gol|
  :option:`-mno-relocatable`  :option:`-mrelocatable-lib`  :option:`-mno-relocatable-lib` |gol|
  :option:`-mtoc`  :option:`-mno-toc`  :option:`-mlittle`  :option:`-mlittle-endian`  :option:`-mbig`  :option:`-mbig-endian` |gol|
  :option:`-mdynamic-no-pic`  :option:`-mswdiv`  :option:`-msingle-pic-base` |gol|
  :option:`-mprioritize-restricted-insns=priority` |gol|
  :option:`-msched-costly-dep=dependence_type` |gol|
  :option:`-minsert-sched-nops=scheme` |gol|
  :option:`-mcall-aixdesc`  :option:`-mcall-eabi`  :option:`-mcall-freebsd`  |gol|
  :option:`-mcall-linux`  :option:`-mcall-netbsd`  :option:`-mcall-openbsd`  |gol|
  :option:`-mcall-sysv`  :option:`-mcall-sysv-eabi`  :option:`-mcall-sysv-noeabi` |gol|
  :option:`-mtraceback=traceback_type` |gol|
  :option:`-maix-struct-return`  :option:`-msvr4-struct-return` |gol|
  :option:`-mabi=abi-type`  :option:`-msecure-plt`  :option:`-mbss-plt` |gol|
  :option:`-mlongcall`  :option:`-mno-longcall`  :option:`-mpltseq`  :option:`-mno-pltseq`  |gol|
  :option:`-mblock-move-inline-limit=num` |gol|
  :option:`-mblock-compare-inline-limit=num` |gol|
  :option:`-mblock-compare-inline-loop-limit=num` |gol|
  :option:`-mno-block-ops-unaligned-vsx` |gol|
  :option:`-mstring-compare-inline-limit=num` |gol|
  :option:`-misel`  :option:`-mno-isel` |gol|
  :option:`-mvrsave`  :option:`-mno-vrsave` |gol|
  :option:`-mmulhw`  :option:`-mno-mulhw` |gol|
  :option:`-mdlmzb`  :option:`-mno-dlmzb` |gol|
  :option:`-mprototype`  :option:`-mno-prototype` |gol|
  :option:`-msim`  :option:`-mmvme`  :option:`-mads`  :option:`-myellowknife`  :option:`-memb`  :option:`-msdata` |gol|
  :option:`-msdata=opt`  :option:`-mreadonly-in-sdata`  :option:`-mvxworks`  :option:`-G` :samp:`{num}` |gol|
  :option:`-mrecip`  :option:`-mrecip=opt`  :option:`-mno-recip`  :option:`-mrecip-precision` |gol|
  :option:`-mno-recip-precision` |gol|
  :option:`-mveclibabi=type`  :option:`-mfriz`  :option:`-mno-friz` |gol|
  :option:`-mpointers-to-nested-functions`  :option:`-mno-pointers-to-nested-functions` |gol|
  :option:`-msave-toc-indirect`  :option:`-mno-save-toc-indirect` |gol|
  :option:`-mpower8-fusion`  :option:`-mno-mpower8-fusion`  :option:`-mpower8-vector`  :option:`-mno-power8-vector` |gol|
  :option:`-mcrypto`  :option:`-mno-crypto`  :option:`-mhtm`  :option:`-mno-htm` |gol|
  :option:`-mquad-memory`  :option:`-mno-quad-memory` |gol|
  :option:`-mquad-memory-atomic`  :option:`-mno-quad-memory-atomic` |gol|
  :option:`-mcompat-align-parm`  :option:`-mno-compat-align-parm` |gol|
  :option:`-mfloat128`  :option:`-mno-float128`  :option:`-mfloat128-hardware`  :option:`-mno-float128-hardware` |gol|
  :option:`-mgnu-attribute`  :option:`-mno-gnu-attribute` |gol|
  :option:`-mstack-protector-guard=guard` :option:`-mstack-protector-guard-reg=reg` |gol|
  :option:`-mstack-protector-guard-offset=offset` :option:`-mprefixed` :option:`-mno-prefixed` |gol|
  :option:`-mpcrel` :option:`-mno-pcrel` :option:`-mmma` :option:`-mno-mmma` :option:`-mrop-protect` :option:`-mno-rop-protect` |gol|
  :option:`-mprivileged` :option:`-mno-privileged`

  *RX Options*

  .. program:: RX

  :option:`-m64bit-doubles`  :option:`-m32bit-doubles`  :option:`-fpu`  :option:`-nofpu` |gol|
  :option:`-mcpu=` |gol|
  :option:`-mbig-endian-data`  :option:`-mlittle-endian-data` |gol|
  :option:`-msmall-data` |gol|
  :option:`-msim`  :option:`-mno-sim` |gol|
  :option:`-mas100-syntax`  :option:`-mno-as100-syntax` |gol|
  :option:`-mrelax` |gol|
  :option:`-mmax-constant-size=` |gol|
  :option:`-mint-register=` |gol|
  :option:`-mpid` |gol|
  :option:`-mallow-string-insns`  :option:`-mno-allow-string-insns` |gol|
  :option:`-mjsr` |gol|
  :option:`-mno-warn-multiple-fast-interrupts` |gol|
  :option:`-msave-acc-in-interrupts`

  *S/390 and zSeries Options*

  .. program:: S/390 and zSeries

  :option:`-mtune=cpu-type`  :option:`-march=cpu-type` |gol|
  :option:`-mhard-float`  :option:`-msoft-float`  :option:`-mhard-dfp`  :option:`-mno-hard-dfp` |gol|
  :option:`-mlong-double-64`  :option:`-mlong-double-128` |gol|
  :option:`-mbackchain`  :option:`-mno-backchain`  :option:`-mpacked-stack`  :option:`-mno-packed-stack` |gol|
  :option:`-msmall-exec`  :option:`-mno-small-exec`  :option:`-mmvcle`  :option:`-mno-mvcle` |gol|
  :option:`-m64`  :option:`-m31`  :option:`-mdebug`  :option:`-mno-debug`  :option:`-mesa`  :option:`-mzarch` |gol|
  :option:`-mhtm`  :option:`-mvx`  :option:`-mzvector` |gol|
  :option:`-mtpf-trace`  :option:`-mno-tpf-trace`  :option:`-mtpf-trace-skip`  :option:`-mno-tpf-trace-skip` |gol|
  :option:`-mfused-madd`  :option:`-mno-fused-madd` |gol|
  :option:`-mwarn-framesize`  :option:`-mwarn-dynamicstack`  :option:`-mstack-size`  :option:`-mstack-guard` |gol|
  :option:`-mhotpatch=halfwords,halfwords`

  *Score Options*

  .. program:: Score

  :option:`-meb`  :option:`-mel` |gol|
  :option:`-mnhwloop` |gol|
  :option:`-muls` |gol|
  :option:`-mmac` |gol|
  :option:`-mscore5`  :option:`-mscore5u`  :option:`-mscore7`  :option:`-mscore7d`

  *SH Options*

  .. program:: SH

  :option:`-m1`  :option:`-m2`  :option:`-m2e` |gol|
  :option:`-m2a-nofpu`  :option:`-m2a-single-only`  :option:`-m2a-single`  :option:`-m2a` |gol|
  :option:`-m3`  :option:`-m3e` |gol|
  :option:`-m4-nofpu`  :option:`-m4-single-only`  :option:`-m4-single`  :option:`-m4` |gol|
  :option:`-m4a-nofpu`  :option:`-m4a-single-only`  :option:`-m4a-single`  :option:`-m4a`  :option:`-m4al` |gol|
  :option:`-mb`  :option:`-ml`  :option:`-mdalign`  :option:`-mrelax` |gol|
  :option:`-mbigtable`  :option:`-mfmovd`  :option:`-mrenesas`  :option:`-mno-renesas`  :option:`-mnomacsave` |gol|
  :option:`-mieee`  :option:`-mno-ieee`  :option:`-mbitops`  :option:`-misize`  :option:`-minline-ic_invalidate`  :option:`-mpadstruct` |gol|
  :option:`-mprefergot`  :option:`-musermode`  :option:`-multcost=number`  :option:`-mdiv=strategy` |gol|
  :option:`-mdivsi3_libfunc=name`  :option:`-mfixed-range=register-range` |gol|
  :option:`-maccumulate-outgoing-args` |gol|
  :option:`-matomic-model=atomic-model` |gol|
  :option:`-mbranch-cost=num`  :option:`-mzdcbranch`  :option:`-mno-zdcbranch` |gol|
  :option:`-mcbranch-force-delay-slot` |gol|
  :option:`-mfused-madd`  :option:`-mno-fused-madd`  :option:`-mfsca`  :option:`-mno-fsca`  :option:`-mfsrra`  :option:`-mno-fsrra` |gol|
  :option:`-mpretend-cmove`  :option:`-mtas`

  *Solaris 2 Options*

  .. program:: Solaris 2

  :option:`-mclear-hwcap`  :option:`-mno-clear-hwcap`  :option:`-mimpure-text`  :option:`-mno-impure-text` |gol|
  :option:`-pthreads`

  *SPARC Options*

  .. program:: SPARC

  :option:`-mcpu=cpu-type` |gol|
  :option:`-mtune=cpu-type` |gol|
  :option:`-mcmodel=code-model` |gol|
  :option:`-mmemory-model=mem-model` |gol|
  :option:`-m32`  :option:`-m64`  :option:`-mapp-regs`  :option:`-mno-app-regs` |gol|
  :option:`-mfaster-structs`  :option:`-mno-faster-structs`  :option:`-mflat`  :option:`-mno-flat` |gol|
  :option:`-mfpu`  :option:`-mno-fpu`  :option:`-mhard-float`  :option:`-msoft-float` |gol|
  :option:`-mhard-quad-float`  :option:`-msoft-quad-float` |gol|
  :option:`-mstack-bias`  :option:`-mno-stack-bias` |gol|
  :option:`-mstd-struct-return`  :option:`-mno-std-struct-return` |gol|
  :option:`-munaligned-doubles`  :option:`-mno-unaligned-doubles` |gol|
  :option:`-muser-mode`  :option:`-mno-user-mode` |gol|
  :option:`-mv8plus`  :option:`-mno-v8plus`  :option:`-mvis`  :option:`-mno-vis` |gol|
  :option:`-mvis2`  :option:`-mno-vis2`  :option:`-mvis3`  :option:`-mno-vis3` |gol|
  :option:`-mvis4`  :option:`-mno-vis4`  :option:`-mvis4b`  :option:`-mno-vis4b` |gol|
  :option:`-mcbcond`  :option:`-mno-cbcond`  :option:`-mfmaf`  :option:`-mno-fmaf`  :option:`-mfsmuld`  :option:`-mno-fsmuld`  |gol|
  :option:`-mpopc`  :option:`-mno-popc`  :option:`-msubxc`  :option:`-mno-subxc` |gol|
  :option:`-mfix-at697f`  :option:`-mfix-ut699`  :option:`-mfix-ut700`  :option:`-mfix-gr712rc` |gol|
  :option:`-mlra`  :option:`-mno-lra`

  *System V Options*

  .. program:: System V

  :option:`-Qy`  :option:`-Qn`  :option:`-YP,paths`  :option:`-Ym,dir`

  *V850 Options*

  .. program:: V850

  :option:`-mlong-calls`  :option:`-mno-long-calls`  :option:`-mep`  :option:`-mno-ep` |gol|
  :option:`-mprolog-function`  :option:`-mno-prolog-function`  :option:`-mspace` |gol|
  :option:`-mtda=n`  :option:`-msda=n`  :option:`-mzda=n` |gol|
  :option:`-mapp-regs`  :option:`-mno-app-regs` |gol|
  :option:`-mdisable-callt`  :option:`-mno-disable-callt` |gol|
  :option:`-mv850e2v3`  :option:`-mv850e2`  :option:`-mv850e1`  :option:`-mv850es` |gol|
  :option:`-mv850e`  :option:`-mv850`  :option:`-mv850e3v5` |gol|
  :option:`-mloop` |gol|
  :option:`-mrelax` |gol|
  :option:`-mlong-jumps` |gol|
  :option:`-msoft-float` |gol|
  :option:`-mhard-float` |gol|
  :option:`-mgcc-abi` |gol|
  :option:`-mrh850-abi` |gol|
  :option:`-mbig-switch`

  *VAX Options*

  .. program:: VAX

  :option:`-mg`  :option:`-mgnu`  :option:`-munix`  :option:`-mlra`

  *Visium Options*

  .. program:: Visium

  :option:`-mdebug`  :option:`-msim`  :option:`-mfpu`  :option:`-mno-fpu`  :option:`-mhard-float`  :option:`-msoft-float` |gol|
  :option:`-mcpu=cpu-type`  :option:`-mtune=cpu-type`  :option:`-msv-mode`  :option:`-muser-mode`

  *VMS Options*

  .. program:: VMS

  :option:`-mvms-return-codes`  :option:`-mdebug-main=prefix`  :option:`-mmalloc64` |gol|
  :option:`-mpointer-size=size`

  *VxWorks Options*

  .. program:: VxWorks

  :option:`-mrtp`  :option:`-non-static`  :option:`-Bstatic`  :option:`-Bdynamic` |gol|
  :option:`-Xbind-lazy`  :option:`-Xbind-now`

  *x86 Options*

  .. program:: x86

  :option:`-mtune=cpu-type`  :option:`-march=cpu-type` |gol|
  :option:`-mtune-ctrl=feature-list`  :option:`-mdump-tune-features`  :option:`-mno-default` |gol|
  :option:`-mfpmath=unit` |gol|
  :option:`-masm=dialect`  :option:`-mno-fancy-math-387` |gol|
  :option:`-mno-fp-ret-in-387`  :option:`-m80387`  :option:`-mhard-float`  :option:`-msoft-float` |gol|
  :option:`-mno-wide-multiply`  :option:`-mrtd`  :option:`-malign-double` |gol|
  :option:`-mpreferred-stack-boundary=num` |gol|
  :option:`-mincoming-stack-boundary=num` |gol|
  :option:`-mcld`  :option:`-mcx16`  :option:`-msahf`  :option:`-mmovbe`  :option:`-mcrc32` :option:`-mmwait` |gol|
  :option:`-mrecip`  :option:`-mrecip=opt` |gol|
  :option:`-mvzeroupper`  :option:`-mprefer-avx128`  :option:`-mprefer-vector-width=opt` |gol|
  :option:`-mmove-max=bits` :option:`-mstore-max=bits` |gol|
  :option:`-mmmx`  :option:`-msse`  :option:`-msse2`  :option:`-msse3`  :option:`-mssse3`  :option:`-msse4.1`  :option:`-msse4.2`  :option:`-msse4`  :option:`-mavx` |gol|
  :option:`-mavx2`  :option:`-mavx512f`  :option:`-mavx512pf`  :option:`-mavx512er`  :option:`-mavx512cd`  :option:`-mavx512vl` |gol|
  :option:`-mavx512bw`  :option:`-mavx512dq`  :option:`-mavx512ifma`  :option:`-mavx512vbmi`  :option:`-msha`  :option:`-maes` |gol|
  :option:`-mpclmul`  :option:`-mfsgsbase`  :option:`-mrdrnd`  :option:`-mf16c`  :option:`-mfma`  :option:`-mpconfig`  :option:`-mwbnoinvd`  |gol|
  :option:`-mptwrite`  :option:`-mprefetchwt1`  :option:`-mclflushopt`  :option:`-mclwb`  :option:`-mxsavec`  :option:`-mxsaves` |gol|
  :option:`-msse4a`  :option:`-m3dnow`  :option:`-m3dnowa`  :option:`-mpopcnt`  :option:`-mabm`  :option:`-mbmi`  :option:`-mtbm`  :option:`-mfma4`  :option:`-mxop` |gol|
  :option:`-madx`  :option:`-mlzcnt`  :option:`-mbmi2`  :option:`-mfxsr`  :option:`-mxsave`  :option:`-mxsaveopt`  :option:`-mrtm`  :option:`-mhle`  :option:`-mlwp` |gol|
  :option:`-mmwaitx`  :option:`-mclzero`  :option:`-mpku`  :option:`-mthreads`  :option:`-mgfni`  :option:`-mvaes`  :option:`-mwaitpkg` |gol|
  :option:`-mshstk` :option:`-mmanual-endbr` :option:`-mcet-switch` :option:`-mforce-indirect-call` |gol|
  :option:`-mavx512vbmi2` :option:`-mavx512bf16` :option:`-menqcmd` |gol|
  :option:`-mvpclmulqdq`  :option:`-mavx512bitalg`  :option:`-mmovdiri`  :option:`-mmovdir64b`  :option:`-mavx512vpopcntdq` |gol|
  :option:`-mavx5124fmaps`  :option:`-mavx512vnni`  :option:`-mavx5124vnniw`  :option:`-mprfchw`  :option:`-mrdpid` |gol|
  :option:`-mrdseed`  :option:`-msgx` :option:`-mavx512vp2intersect` :option:`-mserialize` :option:`-mtsxldtrk` |gol|
  :option:`-mamx-tile`  :option:`-mamx-int8`  :option:`-mamx-bf16` :option:`-muintr` :option:`-mhreset` :option:`-mavxvnni` |gol|
  :option:`-mavx512fp16` :option:`-mavxifma` :option:`-mavxvnniint8` :option:`-mavxneconvert` :option:`-mcmpccxadd` :option:`-mamx-fp16` |gol|
  :option:`-mprefetchi` :option:`-mraoint` |gol|
  :option:`-mcldemote`  :option:`-mms-bitfields`  :option:`-mno-align-stringops`  :option:`-minline-all-stringops` |gol|
  :option:`-minline-stringops-dynamically`  :option:`-mstringop-strategy=alg` |gol|
  :option:`-mkl` :option:`-mwidekl` |gol|
  :option:`-mmemcpy-strategy=strategy`  :option:`-mmemset-strategy=strategy` |gol|
  :option:`-mpush-args`  :option:`-maccumulate-outgoing-args`  :option:`-m128bit-long-double` |gol|
  :option:`-m96bit-long-double`  :option:`-mlong-double-64`  :option:`-mlong-double-80`  :option:`-mlong-double-128` |gol|
  :option:`-mregparm=num`  :option:`-msseregparm` |gol|
  :option:`-mveclibabi=type`  :option:`-mvect8-ret-in-mem` |gol|
  :option:`-mpc32`  :option:`-mpc64`  :option:`-mpc80`  :option:`-mstackrealign` |gol|
  :option:`-momit-leaf-frame-pointer`  :option:`-mno-red-zone`  :option:`-mno-tls-direct-seg-refs` |gol|
  :option:`-mcmodel=code-model`  :option:`-mabi=name`  :option:`-maddress-mode=mode` |gol|
  :option:`-m32`  :option:`-m64`  :option:`-mx32`  :option:`-m16`  :option:`-miamcu`  :option:`-mlarge-data-threshold=num` |gol|
  :option:`-msse2avx`  :option:`-mfentry`  :option:`-mrecord-mcount`  :option:`-mnop-mcount`  :option:`-m8bit-idiv` |gol|
  :option:`-minstrument-return=type` :option:`-mfentry-name=name` :option:`-mfentry-section=name` |gol|
  :option:`-mavx256-split-unaligned-load`  :option:`-mavx256-split-unaligned-store` |gol|
  :option:`-malign-data=type`  :option:`-mstack-protector-guard=guard` |gol|
  :option:`-mstack-protector-guard-reg=reg` |gol|
  :option:`-mstack-protector-guard-offset=offset` |gol|
  :option:`-mstack-protector-guard-symbol=symbol` |gol|
  :option:`-mgeneral-regs-only`  :option:`-mcall-ms2sysv-xlogues` :option:`-mrelax-cmpxchg-loop` |gol|
  :option:`-mindirect-branch=choice`  :option:`-mfunction-return=choice` |gol|
  :option:`-mindirect-branch-register` :option:`-mharden-sls=choice` |gol|
  :option:`-mindirect-branch-cs-prefix` :option:`-mneeded` :option:`-mno-direct-extern-access`

  *x86 Windows Options*

  .. program:: x86 Windows

  :option:`-mconsole`  :option:`-mcygwin`  :option:`-mno-cygwin`  :option:`-mdll` |gol|
  :option:`-mnop-fun-dllimport`  :option:`-mthread` |gol|
  :option:`-municode`  :option:`-mwin32`  :option:`-mwindows`  :option:`-fno-set-stack-executable`

  *Xstormy16 Options*

  .. program:: Xstormy16

  :option:`-msim`

  *Xtensa Options*

  .. program:: Xtensa

  :option:`-mconst16`  :option:`-mno-const16` |gol|
  :option:`-mfused-madd`  :option:`-mno-fused-madd` |gol|
  :option:`-mforce-no-pic` |gol|
  :option:`-mserialize-volatile`  :option:`-mno-serialize-volatile` |gol|
  :option:`-mtext-section-literals`  :option:`-mno-text-section-literals` |gol|
  :option:`-mauto-litpools`  :option:`-mno-auto-litpools` |gol|
  :option:`-mtarget-align`  :option:`-mno-target-align` |gol|
  :option:`-mlongcalls`  :option:`-mno-longcalls` |gol|
  :option:`-mabi=abi-type` |gol|
  :option:`-mextra-l32r-costs=cycles`

  *zSeries Options*

  See :ref:`s-390-and-zseries-options`.

  .. program:: None
