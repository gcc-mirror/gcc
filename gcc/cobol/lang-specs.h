/*
 * Copyright (c) 2021-2025 Symas Corporation
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
        "%{D*} %{E} %{I*} %{M} %{fmax-errors*} %{fsyntax-only*} "
        "%{fcobol-exceptions*} "
        "%{copyext} "
        "%{fexec-charset*} "
        "%{fexec-national-charset*} "
        "%{fstatic-call} %{fdefaultbyte} "
        "%{ffixed-form} %{ffree-form} %{indicator-column*} "
        "%{preprocess} "
        "%{dialect} "
        "%{include} "
	"%{Wno-apply-commit} "
	"%{Wno-any-length} "
	"%{Wno-file-code-set} "
	"%{Wno-high-order-bit} "
	"%{Wno-bad-line-directive} "
	"%{Wno-bad-numeric} "
	"%{Wno-binary-long-long} "
	"%{Wno-call-giving} "
	"%{Wno-call-literal} "
	"%{Wno-cdf-dollar} "
	"%{Wno-cdf-invalid-parameter} "
	"%{Wno-cdf-name-not-found} "
	"%{Wno-cobol-eject} "
	"%{Wno-cobol-resume} "
	"%{Wno-cobol-volatile} "
	"%{Wno-comp-6} "
	"%{Wno-comp-x} "
	"%{Wno-copybook-found} "
	"%{Wno-ec-unknown} "
	"%{Wno-entry-convention} "
	"%{Wno-iconv-error} "
	"%{Wno-include-file-found} "
	"%{Wno-include-file-not-found} "
	"%{Wno-inspect-trailing} "
	"%{Wno-length-of} "
	"%{Wno-level-1-occurs} "
	"%{Wno-level-78} "
	"%{Wno-level-78-defined} "
	"%{Wno-literal-concat} "
	"%{Wno-locale-error} "
	"%{Wno-move-corresponding} "
	"%{Wno-move-index} "
	"%{Wno-move-pointer} "
	"%{Wno-nllanginfo-error} "
	"%{Wno-operator-space} "
	"%{Wno-preprocessor-error} "
	"%{Wno-procedure-not-found} "
	"%{Wno-procedure-pointer} "
	"%{Wno-replace-error} "
	"%{Wno-returning-number} "
	"%{Wno-segment-error} "
	"%{Wno-segment-negative} "
	"%{Wno-stop-number} "
	"%{Wno-stray-indicator} "
	"%{Wno-usage-typename} "
	"%{Wno-recording-mode} "
	"%{Wno-set-locale-to} "
	"%{Wno-set-to-locale} "
        "%{nomain} "
        "%{!fsyntax-only:%(invoke_as)} "
        , 0, 0, 0},
      
