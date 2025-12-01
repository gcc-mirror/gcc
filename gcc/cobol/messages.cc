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

/*
 * Define a table of diagnositic messages, each uniquely identified and 
 * grouped into dialects. The user can select on the command line which 
 * ones are in effect.  
 */

#include <cobol-system.h>
#include <coretypes.h>
#include <tree.h>
#undef yy_flex_debug

#include <langinfo.h>

#include <coretypes.h>
#include <version.h>
#include <demangle.h>
#include <intl.h>
#include <backtrace.h>
#include <diagnostic.h>
#include <opts.h>
#include "util.h"

#include "cbldiag.h"
#include "cdfval.h"
#include "lexio.h"

#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "symbols.h"
#include "inspect.h"
#include "../../libgcobol/io.h"
#include "genapi.h"
#include "genutil.h"
#include "../../libgcobol/charmaps.h"



/*
 * As of now, every diagnositc has one id, one message, one kind, and is
 * associated with "one" dialect. The dialect could be ORed. If it is, that
 * means among the dialects it belongs to, it is always of the same kind.
 *
 * The diagnositic mask in force during compilation may include/exclude
 * features based on their associated dialect and/or by id.  It may stipulate
 * that a warning is treated as an error, too, but that's up the diagnostic
 * framework.  If a feature requires a dialect and is not specifically enabled,
 * gcobol emits of message of the associated kind, and names the dialect
 * required.
 */
struct cbl_diag_t {
  cbl_diag_id_t id;
  cbl_name_t option;
  diagnostics::kind kind;
  cbl_dialect_t dialect;
  
  explicit cbl_diag_t( cbl_diag_id_t id )
    : id(id), option(""), kind(diagnostics::kind::ignored), dialect(dialect_gcc_e)
  {}

  cbl_diag_t( cbl_diag_id_t id,
              const char option[],
              diagnostics::kind kind,
              cbl_dialect_t dialect = dialect_iso_e )
    : id(id), option(""), kind(kind), dialect(dialect)
  {
    gcc_assert(strlen(option) < sizeof(this->option));
    strcpy(this->option, option);
  }

  bool operator<( const cbl_diag_t& that ) const {
    return id < that.id;
  }
};

/*
 * Initially, errors and warnings are set per the default, dialect gcc.  If the
 * user chooses dialect iso, all dialect-enabled features are turned into
 * errors.  If the user selects a more generous dialect, features associated
 * with it are set to be ignored.
 * 
 * Individual features may also be suppressed, and all warnings may be elevated
 * to errors.
 */
const static auto dialect_mf_gnu = cbl_dialect_t(dialect_mf_e | dialect_gnu_e);
const static auto dialect_ibm_mf_gnu = cbl_dialect_t(dialect_ibm_e |
                                                     dialect_mf_e  |
                                                     dialect_gnu_e);

std::set<cbl_diag_t> cbl_diagnostics {
  { CdfNotFoundW, "-Wcdf-name-not-found", diagnostics::kind::warning },
  { CdfParameterW, "-Wcdf-invalid-parameter", diagnostics::kind::warning },

  { EcUnknownW, "-Wec-unknown", diagnostics::kind::warning },

  { IbmEjectE, "-Wcobol-eject", diagnostics::kind::error, dialect_ibm_e },
  { IbmLengthOf, "-Wlength-of", diagnostics::kind::error, dialect_ibm_mf_gnu },
  { IbmEqualAssignE, "-Wequal-assign", diagnostics::kind::error, dialect_ibm_e },
  { IbmProcedurePointer, "-Wprocedure-pointer", diagnostics::kind::error, dialect_ibm_mf_gnu },
  { IbmSectionNegE, "-Wsegment-negative", diagnostics::kind::error, dialect_ibm_e },
  { IbmSectionRangeE, "-Wsegment-error", diagnostics::kind::error, dialect_ibm_e },
  { IbmSectionSegmentW, "-Wsegment", diagnostics::kind::warning, dialect_ibm_e },
  { IbmStopNumber, "-Wstop-number", diagnostics::kind::error, dialect_ibm_e },
  { IbmVolatileE, "-Wcobol-volatile", diagnostics::kind::error, dialect_ibm_e },
  { IbmVolatileW, "-Wcobol-volatile", diagnostics::kind::warning, dialect_ibm_e },

  // RESUME not supported by IBM
  { IsoResume, "-Wcobol-resume", diagnostics::kind::error, dialect_ibm_e },

  { MfBinaryLongLong, "-Wbinary-long-long", diagnostics::kind::error, dialect_mf_gnu },
  { MfCallGiving, "-Wcall-giving", diagnostics::kind::error, dialect_mf_gnu },
  { MfCdfDollar, "-Wcdf-dollar", diagnostics::kind::error, dialect_mf_gnu },
  { MfComp6, "-Wcomp-6", diagnostics::kind::error, dialect_mf_gnu },
  { MfCompX, "-Wcomp-x", diagnostics::kind::error, dialect_mf_gnu },
  { MfLevel_1_Occurs, "Wlevel-1-occurs", diagnostics::kind::error, dialect_mf_gnu },
  { MfLevel78, "-Wlevel-78", diagnostics::kind::error, dialect_mf_gnu },
  { MfMovePointer, "-Wmove-pointer", diagnostics::kind::error, dialect_mf_gnu },
  { MfReturningNum, "-Wreturning-number", diagnostics::kind::error, dialect_mf_gnu },
  { MfUsageTypename, "-Wusage-typename", diagnostics::kind::error, dialect_mf_gnu },
  { MfTrailing, "-Winspect-trailing", diagnostics::kind::error, dialect_mf_gnu },

  { LexIncludeE, "-Winclude-file-not-found", diagnostics::kind::error }, 
  { LexIncludeOkN, "-Winclude-file-found", diagnostics::kind::note }, 
  { LexIndicatorE, "-Wstray-indicator", diagnostics::kind::error }, 
  { LexInputN, "-Wcopybook-found", diagnostics::kind::note }, 
  { LexLineE, "-Wbad-line-directive", diagnostics::kind::error }, 
  { LexPreprocessE, "-Wpreprocessor-error", diagnostics::kind::error }, 
  { LexReplaceE, "-Wreplace-error", diagnostics::kind::error },
  // mf and gnu do not require whitespace after relational operators
  { LexSeparatorE, "-Woperator-space", diagnostics::kind::error, dialect_mf_gnu }, 

  { Par78CdfDefinedW, "-Wlevel-78-defined", diagnostics::kind::warning },
  { ParIconvE, "-Wiconv-error", diagnostics::kind::note }, 
  { ParInfoI, "-Wentry-convention", diagnostics::kind::note }, 
  { ParLangInfoW, "-Wnllanginfo-error", diagnostics::kind::warning },
  { ParLiteral2W, "-Wliteral-concat", diagnostics::kind::warning },
  { ParLocaleW, "-Wlocale-error", diagnostics::kind::warning },
  { ParNoCorrespondingW, "-Wmove-corresponding", diagnostics::kind::warning },
  { ParNumstrW, "-Wbad-numeric", diagnostics::kind::warning },
  { ParUnresolvedProcE, "-Wprocedure-not-found", diagnostics::kind::error },

  // unimplmeneted syntax warnings
  { SynApplyCommit, "-Wapply-commit", diagnostics::kind::warning },
  { SynFileCodeSet, "-Wfile-code-set", diagnostics::kind::warning },
  { SynHighOrderBit, "-Whigh-order-bit", diagnostics::kind::warning },
  { SynRecordingMode, "-Wrecording-mode", diagnostics::kind::warning },
  { SynSetLocaleTo, "-Wset-locale-to", diagnostics::kind::warning },
  { SynSetToLocale, "-Wset-to-locale", diagnostics::kind::warning },

};

static struct set_verify {
  set_verify() {
    gcc_assert(cbl_diagnostics.size() == DiagDiagDiag);
    auto p = std::find_if(cbl_diagnostics.begin(), cbl_diagnostics.end(),
                             []( const auto& diag ) {
                               return '?' == cbl_dialect_str(diag.dialect)[0];
                             } );
    if( p != cbl_diagnostics.end() ) {
      fprintf(stderr, "unregconized dialect '%04x (~%04x)'", p->dialect, ~p->dialect);
    }
    gcc_assert( std::none_of(cbl_diagnostics.begin(), cbl_diagnostics.end(),
                             []( const auto& diag ) {
                               return '?' == cbl_dialect_str(diag.dialect)[0];
                             } ) );
  }
} verify_consistent_message_count;

static inline diagnostics::kind
kind_of( cbl_diag_id_t id ) {
  auto diag = cbl_diagnostics.find(cbl_diag_t(id));
  if( diag != cbl_diagnostics.end() ) {
    return diag->kind;
  }
  return diagnostics::kind::ice;
}  

diagnostics::kind
cbl_diagnostic_kind( cbl_diag_id_t id ) {
  return kind_of(id);
}

bool
cbl_diagnostic_kind( cbl_diag_id_t id, diagnostics::kind kind ) {
  auto p = cbl_diagnostics.find( cbl_diag_t{id} );
  if( p != cbl_diagnostics.end() ) {
    auto diag(*p);
    diag.kind = kind;
    cbl_diagnostics.erase(p);
    return cbl_diagnostics.insert(diag).second;
  }
  return false;
}

bool
cbl_diagnostic_kind( cbl_dialect_t dialect, diagnostics::kind kind ) {
  bool ok = true;
  for( auto diag : cbl_diagnostics ) {
    if( diag.dialect == dialect ) {
      if( ! cbl_diagnostic_kind(diag.id, kind) ) ok = false;
    }
  }
  return ok;
}

void
cobol_warning( cbl_diag_id_t id, int yn, bool warning_as_error ) {
  gcc_assert( 0 <= yn && yn <= 1 );

  diagnostics::kind kind = yn?
    diagnostics::kind::warning : diagnostics::kind::ignored;
  
  if( warning_as_error ) {
    kind = diagnostics::kind::error;
  }
  
  cbl_diagnostic_kind(id, kind);
}

static inline const char *
option_of( cbl_diag_id_t id ) {
  auto diag = cbl_diagnostics.find(cbl_diag_t(id));
  if( diag != cbl_diagnostics.end() && diag->option[0] ) {
    return diag->option;
  }
  return nullptr;
}  

const char *
cbl_diagnostic_option( cbl_diag_id_t id ) {
  return option_of(id);
}

/*
 * This is the general message looker-upper.  It determines whether the
 * diagnositic is in force, at what level, and the message text, and invokes
 * the framework.
 */
extern int yychar;
extern YYLTYPE yylloc;

static const diagnostics::option_id option_zero;

location_t current_token_location();
location_t current_token_location(const location_t& loc);

bool
cbl_message( cbl_diag_id_t id, const char gmsgid[], ... ) {
  auto_diagnostic_group d;
  const char *option;
  char *msg = nullptr;

  diagnostics::kind kind = kind_of(id);
  if( kind == diagnostics::kind::ignored ) return false;

  if( (option = option_of(id)) != nullptr ) {
    msg = xasprintf("%s [%s]", gmsgid, option);
    gmsgid = msg;
  }
 
  va_list ap;

  va_start (ap, gmsgid);
  auto ret = emit_diagnostic_valist( kind, current_token_location(),
                                     option_zero, gmsgid, &ap );
  va_end (ap);
  free(msg);
  
  return ret;
}

bool cbl_message( cbl_loc_t loc, cbl_diag_id_t id, const char gmsgid[], ... ) {
  class temp_loc_t { // copied from util.cc
    location_t orig;
  public:
    temp_loc_t() : orig(current_token_location()) {
      if( yychar < 3 ) return;

      gcc_location_set(yylloc); // use lookahead location
    }
    explicit temp_loc_t( const YYLTYPE& loc) : orig(current_token_location()) {
      gcc_location_set(loc);
    }
    explicit temp_loc_t( const YDFLTYPE& loc) : orig(current_token_location()) {
      gcc_location_set(loc);
    }
    ~temp_loc_t() {
      if( orig != current_token_location() ) {
        current_token_location(orig);
      }
    }
  };

  auto_diagnostic_group d;
  const char *option;
  char *msg = nullptr;

  diagnostics::kind kind = kind_of(id);
  if( kind == diagnostics::kind::ignored ) return false;

  if( (option = option_of(id)) != nullptr ) {
    msg = xasprintf("%s [%s]", gmsgid, option);
    gmsgid = msg;
  }

  temp_loc_t looker(loc);
  va_list ap;
  
  va_start (ap, gmsgid);
  rich_location richloc (line_table, current_token_location());
  auto ret = emit_diagnostic_valist( kind, 
                                     current_token_location(),
                                     option_zero, gmsgid, &ap );
  va_end (ap);
  free(msg);

  return ret;
}

/*
 * Verify the dialect associated with the id (and thus term) is covered by the
 * dialects currently in effect.  If not, issue a standard message of the kind
 * defined by the id. Possible combinations:
 *   dialect required:  ok, dialect matches feature dialect
 *   dialect prohibits  not_ok, dialect matches feature ~dialect
 *
 * If ok is false, then a match means the dialect prohibits the feature. 
 */
bool
dialect_ok( const cbl_loc_t& loc, cbl_diag_id_t id, const char term[], bool ok ) {
  auto diag = cbl_diagnostics.find(cbl_diag_t(id));

  const char *verb = "requires";
  
  if( diag == cbl_diagnostics.end() ) {
    gcc_unreachable();
  }

  if( diag->kind == diagnostics::kind::ignored ) return true;
  
  if( dialect_has(diag->dialect) ) {
    if( ok ) {
      return true;
    } else {
      verb = "prohibits";
    }
  } else {
    if( !ok ) return true; // current dialect correctly does not match the feature
  }

  cbl_message(loc, id, "%qs %s %<-dialect %s%>",
              term, verb, cbl_dialect_str(diag->dialect));
  return false;
}




  
