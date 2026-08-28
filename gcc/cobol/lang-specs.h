/*
 * Copyright (c) 2021-2026 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
/* gcc-src/gcc/config/lang-specs.h */
    {".cob", "@cobol", 0, 0, 0},
    {".COB", "@cobol", 0, 0, 0},
    {".cbl", "@cobol", 0, 0, 0},
    {".CBL", "@cobol", 0, 0, 0},
    {"@cobol",
        "cobol1 %i %(cc1_options) "
        "%{B*} %{D*} %{E} %{I*} %{M} %{fmax-errors*} %{fsyntax-only*} "
        "%{idirafter}"
        "%{fcobol-exceptions*} "
        "%{copyext} "
        "%{fdefaultbyte} "
        "%{fexec-charset*} "
        "%{fexec-national-charset*} "
        "%{ffixed-form} %{ffree-form} %{indicator-column*} "
        "%{fstatic-call} "
        "%{ftrunc} "
        "%{preprocess} "
        "%{dialect} "
        "%{include} "
	"%{Wapply-commit} %{Wno-apply-commit} "
	"%{Wany-length} %{Wno-any-length} "
	"%{Wfile-code-set} %{Wno-file-code-set} "
	"%{Whigh-order-bit} %{Wno-high-order-bit} "
	"%{Wbad-line-directive} %{Wno-bad-line-directive} "
	"%{Wbad-numeric} %{Wno-bad-numeric} "
	"%{Wassign-external} %{Wno-assign-external} "
	"%{Wassign-file} %{Wno-assign-file} "
	"%{Wbinary-long-long} %{Wno-binary-long-long} "
	"%{Wcall-fd} %{Wno-call-fd} "
	"%{Wcall-giving} %{Wno-call-giving} "
	"%{Wcall-literal} %{Wno-call-literal} "
	"%{Wcdf-dollar} %{Wno-cdf-dollar} "
	"%{Wcdf-invalid-parameter} %{Wno-cdf-invalid-parameter} "
	"%{Wcdf-name-not-found} %{Wno-cdf-name-not-found} "
	"%{Wcobol-eject} %{Wno-cobol-eject} "
	"%{Wcobol-resume} %{Wno-cobol-resume} "
	"%{Wcobol-volatile} %{Wno-cobol-volatile} "
	"%{Wcomp-6} %{Wno-comp-6} "
	"%{Wcomp-x} %{Wno-comp-x} "
	"%{Wcopybook-found} %{Wno-copybook-found} "
	"%{Wdisplay-screen} %{Wno-display-screen} "
	"%{Wdynamic-call} %{Wno-dynamic-call} "
	"%{Wec-unknown} %{Wno-ec-unknown} "
	"%{Wentry-convention} %{Wno-entry-convention} "
	"%{Wibm-cdf} %{Wno-ibm-cdf} "
	"%{Wcontent-expr} %{Wno-content-expr} "
	"%{Wiconv-error} %{Wno-iconv-error} "
	"%{Winclude-file-found} %{Wno-include-file-found} "
	"%{Winclude-file-not-found} %{Wno-include-file-not-found} "
	"%{Winspect-trailing} %{Wno-inspect-trailing} "
	"%{Wlength-of} %{Wno-length-of} "
	"%{Wlevel-1-occurs} %{Wno-level-1-occurs} "
	"%{Wlevel-78} %{Wno-level-78} "
	"%{Wlevel-78-defined} %{Wno-level-78-defined} "
	"%{Wliteral-concat} %{Wno-literal-concat} "
	"%{Wlocale-error} %{Wno-locale-error} "
	"%{Wmove-corresponding} %{Wno-move-corresponding} "
	"%{Wmove-index} %{Wno-move-index} "
	"%{Wmove-pointer} %{Wno-move-pointer} "
	"%{Wnllanginfo-error} %{Wno-nllanginfo-error} "
	"%{Woperator-space} %{Wno-operator-space} "
	"%{Wpreprocessor-error} %{Wno-preprocessor-error} "
	"%{Wprocedure-not-found} %{Wno-procedure-not-found} "
	"%{Wprocedure-pointer} %{Wno-procedure-pointer} "
	"%{Wreplace-error} %{Wno-replace-error} "
	"%{Wredefines-first} %{Wno-redefines-first} "
	"%{Wredefines-table} %{Wno-redefines-table} "
	"%{Wredefines-grow} %{Wno-redefines-grow} "
	"%{Wreturning-number} %{Wno-returning-number} "
	"%{Wsegment-error} %{Wno-segment-error} "
	"%{Wsegment-negative} %{Wno-segment-negative} "
	"%{Wset-numeric} %{Wno-set-numeric} "
	"%{Wstop-number} %{Wno-stop-number} "
	"%{Wstray-indicator} %{Wno-stray-indicator} "
	"%{Wusage-typename} %{Wno-usage-typename} "
	"%{Wrecording-mode} %{Wno-recording-mode} "
	"%{Wset-locale-to} %{Wno-set-locale-to} "
	"%{Wset-to-locale} %{Wno-set-to-locale} "
        "%{nomain} "
        "%{!fsyntax-only:%(invoke_as)} "
        , 0, 0, 0},
      
