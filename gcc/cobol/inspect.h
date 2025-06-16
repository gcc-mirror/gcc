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

struct cbl_inspect_qual_t {
  bool initial;
  cbl_refer_t identifier_4;

  cbl_inspect_qual_t() : initial(false), identifier_4(cbl_refer_t()) {}
  cbl_inspect_qual_t( bool initial, const cbl_refer_t& identifier_4 )
    : initial(initial), identifier_4(identifier_4)
    {}
  cbl_inspect_qual_t( const cbl_inspect_qual_t& that )
    : initial(that.initial)
    , identifier_4(that.identifier_4)
    {}

  cbl_inspect_qual_t& operator=( const cbl_inspect_qual_t& that ) {
    initial = that.initial;
    identifier_4 = that.identifier_4;
    return *this;
  }

  bool active() const { return is_active(identifier_4); }
};

/*
 * Data for INSPECT X TALLYING Y FOR.  Captures information for operands of
 * CHARACTERS/ALL/LEADING.  The CHARACTERS/ALL/LEADING control is kept at the
 * next higher level, and may be repeated for each tally.
 *
 * cbl_inspect_match_t::matching is not used with CHARACTERS
 */

class cbl_inspect_match_t {
  friend void dump_inspect_match( const cbl_inspect_match_t& M );
  cbl_refer_t match;  // identifier-3/5 or literal-1/3
  cbl_refer_t tally;  // collected too soon, belongs to next phrase 
 public:
  cbl_inspect_qual_t before, after; // phrase 1

  cbl_inspect_match_t() {}
  explicit 
  cbl_inspect_match_t( const cbl_refer_t& matching,
                       const cbl_inspect_qual_t& before = cbl_inspect_qual_t(),
                       const cbl_inspect_qual_t& after  = cbl_inspect_qual_t() )
    : match(matching)
    , before(before)
    , after(after)
  {}
  // match all characters
  bool match_any() const { return !(before.active() || after.active()); }

  void save_premature_tally( const cbl_refer_t& tally ) {
    this->tally = tally; // put it here temporarily
  }
  cbl_refer_t premature_tally() {
    if( !tally.field ) { std::swap(match, tally); }
    return tally;
  }
  
  const cbl_refer_t& matching( const cbl_refer_t& match ) {
    return this->match = match;
  }
  const cbl_refer_t& matching() const { return match; }

  bool empty() const {
    return !is_active(match) && !before.active() && !after.active();
  }
};

/*
 * Data for INSPECT X REPLACING.  The CHARACTERS/ALL/LEADING/FIRST control is
 * kept at the next higher level, and may be repeated.
 */
struct cbl_inspect_replace_t : public cbl_inspect_match_t {
  cbl_refer_t replacement;

  cbl_inspect_replace_t() {}
  cbl_inspect_replace_t( const cbl_refer_t& matching,
                         const cbl_refer_t& replacement,
                         const cbl_inspect_qual_t& before,
                         const cbl_inspect_qual_t& after )
    : cbl_inspect_match_t(matching, before, after)
    , replacement(replacement)
  {}
};

// One partial tally or substitution.
struct cbl_inspect_oper_t {
  cbl_inspect_bound_t bound;  // CHARACTERS/ALL/LEADING/FIRST
  // either tallies or replaces is empty
  std::vector<cbl_inspect_match_t> matches;
  std::vector<cbl_inspect_replace_t> replaces;

  cbl_inspect_oper_t() : bound(bound_characters_e) {}

  explicit cbl_inspect_oper_t( const cbl_inspect_match_t& match,
                               cbl_inspect_bound_t bound = bound_characters_e )
    : bound(bound)
  {
    matches.push_back(match);
  }
  explicit cbl_inspect_oper_t( const cbl_inspect_replace_t& replace,
                               cbl_inspect_bound_t bound = bound_characters_e )
    : bound(bound)
  {
    replaces.push_back(replace);
  }

  cbl_inspect_oper_t( cbl_inspect_bound_t bound,
                      const std::vector<cbl_inspect_match_t>& matches )
    : bound(bound)
    , matches(matches)
    {}

  cbl_inspect_oper_t( cbl_inspect_bound_t bound,
                      const std::vector<cbl_inspect_replace_t>& replaces )
    : bound(bound)
    , replaces(replaces)
    {}

  // N matches/replaces
  size_t n_identifier_3() const {
    return std::max( matches.size(), replaces.size() );
  } 

  bool is_valid() const { // only one or the other, never both
    bool invalid = !matches.empty() && !replaces.empty();
    return ! invalid;
  }
};

// One whole tally or substitution.  For REPLACING, nbound == 1
// FOR and REPLACING start with a cbl_inspect_bound_t
struct cbl_inspect_t : public std::vector<cbl_inspect_oper_t> {
  cbl_refer_t tally; // field is NULL for REPLACING
  cbl_inspect_t() {}
  cbl_inspect_t( size_t n, const cbl_inspect_oper_t& oper )
    : std::vector<cbl_inspect_oper_t>(n, oper)
  {}
  cbl_inspect_t( const cbl_refer_t& tally,
                 const std::vector<cbl_inspect_oper_t>& opers )
    : std::vector<cbl_inspect_oper_t>(opers)
    , tally(tally)
    {}

  size_t nbound() const { return size(); }
};

typedef std::vector<cbl_inspect_t> cbl_inspect_opers_t;

/*
 * Runtime
 */

void parser_inspect( const cbl_refer_t& input,
                     bool backward,
                     cbl_inspect_opers_t& inspects );

void parser_inspect_conv( cbl_refer_t input, bool backward,
                          cbl_refer_t original,
                          cbl_refer_t replacement,
                          cbl_inspect_qual_t before = cbl_inspect_qual_t(),
                          cbl_inspect_qual_t after =  cbl_inspect_qual_t() );

#endif // INSPECT_H
