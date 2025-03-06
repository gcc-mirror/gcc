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
#ifndef INSPECT_H
#define INSPECT_H
#include <algorithm>
#include <cstddef>
#include <cstring>
#include <cstdio>

/*
 * INSPECT has 3 repeating elements:
 *
 *   1. cbl_inspect_t
 *      Tally (identifier-2).  parser_inspect takes N of these.
 *      Because REPLACING has no such loop, N == 1 for REPLACING.
 *
 *   2. cbl_inspect_oper_t
 *      The CHARACTERS/ALL/LEADING/FIRST phrase (type of match)
 *      Has N match/replace operands (or both)
 *
 *   3. cbl_inspect_match_t and cbl_inspect_replace_t
 *      The CHARACTERS/ALL/LEADING/FIRST operands
 *      Has N tuples of identifier-3 + [BEFORE and/or AFTER]
 */

static inline bool
is_active( const cbl_refer_t& refer ) { return NULL != refer.field; }

template <typename DATA>
struct cbx_inspect_qual_t {
  bool initial;
  DATA identifier_4;

  cbx_inspect_qual_t() : initial(false), identifier_4(DATA()) {}
  cbx_inspect_qual_t( bool initial, const DATA& identifier_4 )
    : initial(initial), identifier_4(identifier_4)
    {
      //if( identifier_4.field ) yywarn("%s:qualifying field is '%s'", __func__, identifier_4.field->name);
    }
  cbx_inspect_qual_t( const cbx_inspect_qual_t& that )
    : initial(that.initial)
    , identifier_4(that.identifier_4)
    {
      //if( identifier_4.field ) yywarn("%s:qualifying field is '%s'", __func__, identifier_4.field->name);
    }

  cbx_inspect_qual_t& operator=( const cbx_inspect_qual_t& that ) {
    initial = that.initial;
    identifier_4 = that.identifier_4;
    //if( identifier_4.field ) yywarn("%s:qualifying field is '%s'", __func__, identifier_4.field->name);
    return *this;
  }

  bool active() const { return is_active(identifier_4); }

  void clear() {
    initial = false;
    identifier_4.clear();
  }
};

typedef cbx_inspect_qual_t<cbl_refer_t> cbl_inspect_qual_t;

/*
 * Data for INSPECT X TALLYING Y FOR.  Captures information for operands of
 * CHARACTERS/ALL/LEADING.  The CHARACTERS/ALL/LEADING control is kept at the
 * next higher level, and may be repeated for each tally.
 *
 * cbx_inspect_match_t::matching is not used with CHARACTERS
 */
template <typename DATA>
struct cbx_inspect_match_t {
  DATA matching;                          // identifier-3/5 or literal-1/3
  cbx_inspect_qual_t<DATA> before, after; // phrase 1

  cbx_inspect_match_t(
    const DATA& matching = DATA(),
    cbx_inspect_qual_t<DATA> before = cbx_inspect_qual_t<DATA>(),
    cbx_inspect_qual_t<DATA> after = cbx_inspect_qual_t<DATA>()
    )
    : matching(matching)
    , before(before)
    , after(after)
  {}
  // match all characters
  bool match_any() const { return !(before.active() || after.active()); }
};

typedef cbx_inspect_match_t<cbl_refer_t> cbl_inspect_match_t;

/*
 * Data for INSPECT X REPLACING.  The CHARACTERS/ALL/LEADING/FIRST control is
 * kept at the next higher level, and may be repeated.
 */
template <typename DATA>
struct cbx_inspect_replace_t : public cbx_inspect_match_t<DATA> {
  DATA replacement;

  cbx_inspect_replace_t( const DATA& matching = DATA(),
                         const DATA& replacement = DATA() )
    : cbx_inspect_match_t<DATA>(matching)
    , replacement(replacement)
  {}
  cbx_inspect_replace_t( const DATA& matching,
                         const DATA& replacement,
                         const cbx_inspect_qual_t<DATA>& before,
                         const cbx_inspect_qual_t<DATA>& after )
    : cbx_inspect_match_t<DATA>(matching, before, after)
    , replacement(replacement)
  {}
};

typedef cbx_inspect_replace_t<cbl_refer_t> cbl_inspect_replace_t;

// One partial tally or substitution.
template <typename DATA>
struct cbx_inspect_oper_t {
  cbl_inspect_bound_t bound;  // CHARACTERS/ALL/LEADING/FIRST
  size_t n_identifier_3;      // N matches/replaces
  // either tallies or replaces is NULL
  cbx_inspect_match_t<DATA> *matches;
  cbx_inspect_replace_t<DATA> *replaces;

  cbx_inspect_oper_t( cbl_inspect_bound_t bound,
                      std::list<cbx_inspect_match_t<DATA>> matches )
    : bound(bound)
    , n_identifier_3( matches.size())
    , matches(NULL)
    , replaces(NULL)
    {
      this->matches = new cbx_inspect_match_t<DATA>[n_identifier_3];
      std::copy( matches.begin(), matches.end(), this->matches );
    }

  cbx_inspect_oper_t( cbl_inspect_bound_t bound,
                      std::list<cbx_inspect_replace_t<DATA>> replaces )
    : bound(bound)
    , n_identifier_3( replaces.size() )
    , matches(NULL)
    , replaces(NULL)
    {
      this->replaces = new cbx_inspect_replace_t<DATA>[n_identifier_3];
      std::copy( replaces.begin(), replaces.end(), this->replaces );
    }

  cbx_inspect_oper_t()
    : bound(bound_characters_e)
    , n_identifier_3(0)
    , matches(NULL)
    , replaces(NULL)
    {
      assert( is_valid() );
    }

  bool is_valid() const {
    if( matches && replaces ) return false;
    if( matches || replaces ) return n_identifier_3 > 0;
    return n_identifier_3 == 0;
  }
};

typedef cbx_inspect_oper_t<cbl_refer_t> cbl_inspect_oper_t;

// One whole tally or substitution.  For REPLACING, nbound == 1
template <typename DATA>
struct cbx_inspect_t {
  DATA tally;                 // identifier-2: NULL without a tally
  size_t nbound;              // Each FOR or REPLACING operation starts with a cbl_inspect_bound_t
  cbx_inspect_oper_t<DATA> *opers;

  cbx_inspect_t( const DATA& tally = DATA() )
    : tally(tally)
    , nbound(0)
    , opers(NULL)
    {}
  cbx_inspect_t( const DATA& tally, cbx_inspect_oper_t<DATA> oper )
    : tally(tally)
    , nbound(1)
    , opers(NULL)
    {
      this->opers = new cbx_inspect_oper_t<DATA>[1];
      this->opers[0] = oper;
    }
  cbx_inspect_t( const DATA& tally,
                 const std::list<cbx_inspect_oper_t<DATA>>& opers )
    : tally(tally)
    , nbound( opers.size() )
    , opers(NULL)
    {
      this->opers = new cbx_inspect_oper_t<DATA>[nbound];
      std::copy( opers.begin(), opers.end(), this->opers );
    }
};

typedef cbx_inspect_t<cbl_refer_t> cbl_inspect_t;


/*
 * Runtime
 */

void parser_inspect( cbl_refer_t input, bool backward,
                     size_t ninspect, cbl_inspect_t *inspects );
void parser_inspect_conv( cbl_refer_t input, bool backward,
                          cbl_refer_t original,
                          cbl_refer_t replacement,
                          cbl_inspect_qual_t before = cbl_inspect_qual_t(),
                          cbl_inspect_qual_t after =  cbl_inspect_qual_t() );

#endif // INSPECT_H
