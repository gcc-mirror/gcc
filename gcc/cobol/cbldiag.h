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

#ifdef _CBLDIAG_H
#pragma message __FILE__ " included twice"
#else
#define _CBLDIAG_H

#if GCOBOL_GETENV
#define gcobol_getenv(x) getenv(x)
#else
#define gcobol_getenv(x) ((char *)nullptr)
#endif

const char * cobol_filename();

#if ! defined(ATTRIBUTE_GCOBOL_DIAG)
#define ATTRIBUTE_GCOBOL_DIAG(a, b)
#endif
#if ! defined(ATTRIBUTE_PRINTF_1)
#define ATTRIBUTE_PRINTF_1
#endif
#if ! defined(ATTRIBUTE_PRINTF_3)
#define ATTRIBUTE_PRINTF_3
#endif

/*
 * Enumerations do not depend on anything else. 
 */

enum cbl_gcobol_feature_t {
  feature_gcc_e = 0x00,
  feature_internal_ebcdic_e = 0x01,
  feature_embiggen_e        = 0x02, // widen numeric that redefine POINTER
};

bool cobol_gcobol_feature_set( cbl_gcobol_feature_t gcobol_feature,
                               bool on = true );

enum cbl_call_convention_t {
  cbl_call_verbatim_e = 'V',
  cbl_call_cobol_e = 'N', // native
};

void current_call_convention( cbl_call_convention_t convention);
cbl_call_convention_t current_call_convention();


/*
 * CDF state does not require types that would be defined in another file. 
 */
void cdf_push();
void cdf_push_call_convention();
void cdf_push_current_tokens();
void cdf_push_dictionary();
void cdf_push_enabled_exceptions();

void cdf_pop();
void cdf_pop_call_convention();
void cdf_pop_current_tokens();
void cdf_pop_dictionary();
void cdf_pop_enabled_exceptions();

size_t current_program_index();

/*
 *  These are user-facing messages.  They go through the gcc
 *  diagnostic framework and use text that can be localized.
 */
// cppcheck-suppress syntaxError
void yyerror( const char fmt[], ... ) ATTRIBUTE_GCOBOL_DIAG(1, 2);

struct cbl_loc_base_t {
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
struct cbl_loc_t : public cbl_loc_base_t {

  cbl_loc_t() = default; // cppcheck-suppress uninitDerivedMemberVar

  cbl_loc_t(   int first_line, int first_column,
               int last_line,  int last_column ) 
    : cbl_loc_base_t {
        first_line , first_column,
        last_line, last_column
      }
  {}
  // cppcheck-suppress noExplicitConstructor
  cbl_loc_t( const cbl_loc_base_t& base )
    : cbl_loc_base_t(base)   
  {}

  explicit cbl_loc_t(   int line )
    : cbl_loc_base_t {
        line, 1, line, 1
      }
  {}

  cbl_loc_t& operator=( int lineno ) {
    first_line = last_line = lineno;
    first_column = last_column = 1;
    return *this;
  }

  // Represent a multi-line location as the first line, so "included from"
  // reflects where the COPY statement appears, not where it ends. 
  cbl_loc_t as_first_line() const {
    cbl_loc_t loc(*this);
    loc.last_line = first_line;
    loc.last_column = first_column;
    return loc;
  }
};

#include <type_traits>
/* allow relocate stack */
static_assert(std::is_trivially_copyable<cbl_loc_t>::value,
             "cbl_loc_t must be trivially copyable for parser stack growth");

const cbl_loc_t& cobol_location();

/*
 * Naming Convention: Names end with a letter that indicates 
 * their kind:
 * F  fatal, "fatal error: "
 * I  ice, "internal compiler error: "
 * E  error, "error: "
 * S  sorry, "sorry, unimplemented: "
 * W  warning, "warning: "
 * A  anachronism, "anachronism: "
 * N  note, "note: "
 * D  debug, "debug: "
 */
enum cbl_diag_id_t : uint64_t {
  CdfNotFoundW, 
  CdfParameterW,
  
  EcUnknownW, 

  LexIncludeE,
  LexIncludeOkN,
  LexIndicatorE,
  LexInputN,
  LexLineE,
  LexPreprocessE,
  LexReplaceE,
  LexSeparatorE,

  IbmCallFd,
  IbmCdf,
  IbmContentExpr,
  IbmEjectE,
  IbmEqualAssignE,
  IbmLengthOf, 
  IbmProcedurePointer,
  IbmSectionNegE,
  IbmSectionRangeE,
  IbmSectionSegmentW,
  IbmStopNumber,
  IbmVolatileE,  
  IbmVolatileW,  // dialect warning for ignored syntax

  IsoAssignFile,
  IsoRedefinesGrow,
  IsoResume,

  MfAssignExternal,
  MfBinaryLongLong,
  MfCallGiving,
  MfCallLiteral,
  MfCdfDollar, 
  MfComp6,
  MfCompX,
  MfLevel_1_Occurs, 
  MfLevel78,
  MfAnyLength, 
  MfMoveIndex, 
  MfMovePointer, 
  MfRedefinesFirst,
  MfReturningNum,
  MfSetNumeric,
  MfTrailing,
  MfUsageTypename,
  
  Par78CdfDefinedW,
  ParDynamicCall,
  ParIconvE, 
  ParInfoI,
  ParLangInfoW,
  ParLiteral2W, 
  ParLocaleW,
  ParNoCorrespondingW,
  ParNumstrW, 
  ParUnresolvedProcE, 

  SynApplyCommit,
  SynFileCodeSet,
  SynHighOrderBit,
  SynRecordingMode,
  SynSetLocaleTo,
  SynSetToLocale,

  DiagDiagDiag // always last
};

void cbl_err(const char *format_string, ...) ATTRIBUTE_GCOBOL_DIAG(1, 2);
void cbl_errx(const char *format_string, ...) ATTRIBUTE_GCOBOL_DIAG(1, 2);

bool cbl_message( cbl_diag_id_t id, const char msg[], ... )
  ATTRIBUTE_GCOBOL_DIAG(2, 3);

bool cbl_message( cbl_loc_t loc, cbl_diag_id_t id, const char msg[], ... )
  ATTRIBUTE_GCOBOL_DIAG(3, 4);

[[noreturn]] void cbl_internal_error(const char *format_string, ...)
  ATTRIBUTE_GCOBOL_DIAG(1, 2);

bool
dialect_ok( const cbl_loc_t& loc, cbl_diag_id_t id, const char term[], bool ok = true );

static inline bool
dialect_not_ok( const cbl_loc_t& loc, cbl_diag_id_t id, const char term[] ) {
  return dialect_ok(loc, id, term, false);
}

bool cbl_diagnostic_ignored( cbl_diag_id_t id );

// Diagnostic format specifiers are documented in gcc/pretty-print.cc
// an error at a location, called from the parser for semantic errors
void error_msg( const cbl_loc_t& loc, const char gmsgid[], ... )
  ATTRIBUTE_GCOBOL_DIAG(2, 3);

void error_msg( const cbl_loc_t& loc, const char gmsgid[], ... )
  ATTRIBUTE_GCOBOL_DIAG(2, 3);

bool
warn_msg( const cbl_loc_t& loc, const char gmsgid[], ... )
  ATTRIBUTE_GCOBOL_DIAG(2, 3);

// an error that uses token_location, not yylloc
void error_msg_direct( const char gmsgid[], ... )
  ATTRIBUTE_GCOBOL_DIAG(1, 2);

// for CDF and other warnings that refer back to an earlier line
// (not in diagnostic framework yet)
void yyerrorvl( int line, const char *filename, const char fmt[], ... )
  ATTRIBUTE_PRINTF_3;

void cbl_unimplementedw(cbl_diag_id_t id, const char *gmsgid, ...)
  ATTRIBUTE_GCOBOL_DIAG(2, 3); // warning
void cbl_unimplemented(const char *gmsgid, ...)
  ATTRIBUTE_GCOBOL_DIAG(1, 2);  // error
void cbl_unimplemented_at( const  cbl_loc_t& loc, const char *gmsgid, ... )
  ATTRIBUTE_GCOBOL_DIAG(2, 3);

/*
 * dbgmsg produce messages not intended for the user.  They cannot be localized
 * and fwrite directly to standard error.  dbgmsg is activated by -fflex-debug
 * or -fyacc-debug.
 */
void dbgmsg( const char fmt[], ... ) ATTRIBUTE_PRINTF_1;

void gcc_location_set( const cbl_loc_t& loc );

void gcc_location_dump();

void
location_dump( const char func[], int line, const char tag[],
               const cbl_loc_t& loc, bool force = false);
#endif
