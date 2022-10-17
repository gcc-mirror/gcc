/* C++ code produced by gperf version 3.1 */
/* Command-line: gperf -o -C -E -D -N find -L C++ --output-file std-name-hint.h -k'1,2,7,11,$' std-name-hint.gperf  */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gperf@gnu.org>."
#endif

#line 4 "std-name-hint.gperf"

/* Copyright (C) 2022 Free Software Foundation, Inc.

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
#line 23 "std-name-hint.gperf"
struct std_name_hint
{
  /* A name within "std::".  */
  const char *name;

  /* The header name defining it within the C++ Standard Library
     (with '<' and '>').  */
  const char* header;

  /* The dialect of C++ in which this was added.  */
  enum cxx_dialect min_dialect;
};
/* maximum key range = 626, duplicates = 4 */

class std_name_hint_lookup
{
private:
  static inline unsigned int hash (const char *str, size_t len);
public:
  static const struct std_name_hint *find (const char *str, size_t len);
};

inline unsigned int
std_name_hint_lookup::hash (const char *str, size_t len)
{
  static const unsigned short asso_values[] =
    {
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635,   5,
      635,   0, 635, 635, 635, 635,  25, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635,  45, 635,  25,  70, 165,
       20,   0,  35, 225, 190,  95, 635, 120, 183,  10,
        5,  25, 165,   5,   5,  10,   0,  55,   4, 143,
      115, 229, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635, 635, 635, 635, 635,
      635, 635, 635, 635, 635, 635
    };
  unsigned int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[static_cast<unsigned char>(str[10])];
      /*FALLTHROUGH*/
      case 10:
      case 9:
      case 8:
      case 7:
        hval += asso_values[static_cast<unsigned char>(str[6])];
      /*FALLTHROUGH*/
      case 6:
      case 5:
      case 4:
      case 3:
      case 2:
        hval += asso_values[static_cast<unsigned char>(str[1])];
      /*FALLTHROUGH*/
      case 1:
        hval += asso_values[static_cast<unsigned char>(str[0])];
        break;
    }
  return hval + asso_values[static_cast<unsigned char>(str[len - 1])];
}

const struct std_name_hint *
std_name_hint_lookup::find (const char *str, size_t len)
{
  enum
    {
      TOTAL_KEYWORDS = 205,
      MIN_WORD_LENGTH = 2,
      MAX_WORD_LENGTH = 22,
      MIN_HASH_VALUE = 9,
      MAX_HASH_VALUE = 634
    };

  static const struct std_name_hint wordlist[] =
    {
#line 158 "std-name-hint.gperf"
      {"next", "<iterator>", cxx11},
#line 212 "std-name-hint.gperf"
      {"set", "<set>", cxx98},
#line 287 "std-name-hint.gperf"
      {"vector", "<vector>", cxx98},
#line 101 "std-name-hint.gperf"
      {"setbase", "<iomanip>", cxx98},
#line 165 "std-name-hint.gperf"
      {"ends", "<ostream>", cxx98},
#line 205 "std-name-hint.gperf"
      {"ends", "<ostream>", cxx98},
#line 86 "std-name-hint.gperf"
      {"mem_fn", "<functional>", cxx11},
#line 69 "std-name-hint.gperf"
      {"deque", "<deque>", cxx98},
#line 162 "std-name-hint.gperf"
      {"reverse_iterator", "<iterator>", cxx98},
#line 151 "std-name-hint.gperf"
      {"end", "<iterator>", cxx11},
#line 264 "std-name-hint.gperf"
      {"void_t", "<type_traits>", cxx17},
#line 284 "std-name-hint.gperf"
      {"variant", "<variant>", cxx17},
#line 281 "std-name-hint.gperf"
      {"move", "<utility>", cxx11},
#line 87 "std-name-hint.gperf"
      {"not_fn", "<functional>", cxx17},
#line 269 "std-name-hint.gperf"
      {"negation", "<type_traits>", cxx17},
#line 270 "std-name-hint.gperf"
      {"negation_v", "<type_traits>", cxx17},
#line 122 "std-name-hint.gperf"
      {"nouppercase", "<ios>", cxx98},
#line 43 "std-name-hint.gperf"
      {"any_cast", "<any>", cxx17},
#line 181 "std-name-hint.gperf"
      {"make_unique", "<memory>", cxx14},
#line 147 "std-name-hint.gperf"
      {"advance", "<iterator>", cxx98},
#line 157 "std-name-hint.gperf"
      {"move_iterator", "<iterator>", cxx11},
#line 246 "std-name-hint.gperf"
      {"make_from_tuple", "<tuple>", cxx17},
#line 134 "std-name-hint.gperf"
      {"defaultfloat", "<ios>", cxx11},
#line 249 "std-name-hint.gperf"
      {"tuple", "<tuple>", cxx11},
#line 257 "std-name-hint.gperf"
      {"enable_if_t", "<type_traits>", cxx14},
#line 164 "std-name-hint.gperf"
      {"ostream", "<ostream>", cxx98},
#line 203 "std-name-hint.gperf"
      {"ostream", "<ostream>", cxx98},
#line 261 "std-name-hint.gperf"
      {"remove_cvref_t", "<type_traits>", cxx20},
#line 209 "std-name-hint.gperf"
      {"queue", "<queue>", cxx98},
#line 159 "std-name-hint.gperf"
      {"ostream_iterator", "<iterator>", cxx98},
#line 227 "std-name-hint.gperf"
      {"stringstream", "<sstream>", cxx98},
#line 251 "std-name-hint.gperf"
      {"tuple_element", "<tuple>", cxx11},
#line 252 "std-name-hint.gperf"
      {"tuple_element_t", "<tuple>", cxx14},
#line 77 "std-name-hint.gperf"
      {"fstream", "<fstream>", cxx98},
#line 213 "std-name-hint.gperf"
      {"multiset", "<set>", cxx98},
#line 280 "std-name-hint.gperf"
      {"make_pair", "<utility>", cxx98},
#line 253 "std-name-hint.gperf"
      {"tuple_size", "<tuple>", cxx11},
#line 100 "std-name-hint.gperf"
      {"setiosflags", "<iomanip>", cxx98},
#line 99 "std-name-hint.gperf"
      {"resetiosflags", "<iomanip>", cxx98},
#line 149 "std-name-hint.gperf"
      {"begin", "<iterator>", cxx11},
#line 109 "std-name-hint.gperf"
      {"quoted", "<iomanip>", cxx14},
#line 275 "std-name-hint.gperf"
      {"unordered_set", "<unordered_set>", cxx11},
#line 276 "std-name-hint.gperf"
      {"unordered_multiset", "<unordered_set>", cxx11},
#line 256 "std-name-hint.gperf"
      {"enable_if", "<type_traits>", cxx11},
#line 95 "std-name-hint.gperf"
      {"future", "<future>", cxx11},
#line 260 "std-name-hint.gperf"
      {"remove_cvref", "<type_traits>", cxx20},
#line 248 "std-name-hint.gperf"
      {"tie", "<tuple>", cxx11},
#line 247 "std-name-hint.gperf"
      {"make_tuple", "<tuple>", cxx11},
#line 71 "std-name-hint.gperf"
      {"forward_list", "<forward_list>", cxx11},
#line 79 "std-name-hint.gperf"
      {"ofstream", "<fstream>", cxx98},
#line 285 "std-name-hint.gperf"
      {"visit", "<variant>", cxx17},
#line 127 "std-name-hint.gperf"
      {"right", "<ios>", cxx98},
#line 85 "std-name-hint.gperf"
      {"invoke", "<functional>", cxx17},
#line 279 "std-name-hint.gperf"
      {"forward", "<utility>", cxx11},
#line 114 "std-name-hint.gperf"
      {"noshowbase", "<ios>", cxx98},
#line 153 "std-name-hint.gperf"
      {"inserter", "<iterator>", cxx98},
#line 160 "std-name-hint.gperf"
      {"ostreambuf_iterator", "<iterator>", cxx98},
#line 51 "std-name-hint.gperf"
      {"atomic_ref", "<atomic>", cxx20},
#line 112 "std-name-hint.gperf"
      {"noboolalpha", "<ios>", cxx98},
#line 148 "std-name-hint.gperf"
      {"back_inserter", "<iterator>", cxx98},
#line 183 "std-name-hint.gperf"
      {"unique_ptr", "<memory>", cxx11},
#line 89 "std-name-hint.gperf"
      {"unwrap_reference", "<functional>", cxx20},
#line 90 "std-name-hint.gperf"
      {"unwrap_reference_t", "<functional>", cxx20},
#line 219 "std-name-hint.gperf"
      {"source_location", "<source_location>", cxx20},
#line 254 "std-name-hint.gperf"
      {"tuple_size_v", "<tuple>", cxx17},
#line 83 "std-name-hint.gperf"
      {"function", "<functional>", cxx11},
#line 144 "std-name-hint.gperf"
      {"istream", "<istream>", cxx98},
#line 229 "std-name-hint.gperf"
      {"stack", "<stack>", cxx98},
#line 154 "std-name-hint.gperf"
      {"istream_iterator", "<iterator>", cxx98},
#line 123 "std-name-hint.gperf"
      {"unitbuf", "<ios>", cxx98},
#line 224 "std-name-hint.gperf"
      {"basic_stringstream", "<sstream>", cxx98},
#line 245 "std-name-hint.gperf"
      {"forward_as_tuple", "<tuple>", cxx11},
#line 124 "std-name-hint.gperf"
      {"nounitbuf", "<ios>", cxx98},
#line 119 "std-name-hint.gperf"
      {"skipws", "<ios>", cxx98},
#line 75 "std-name-hint.gperf"
      {"basic_ofstream", "<fstream>", cxx98},
#line 156 "std-name-hint.gperf"
      {"iterator_traits", "<iterator>", cxx98},
#line 76 "std-name-hint.gperf"
      {"basic_fstream", "<fstream>", cxx98},
#line 131 "std-name-hint.gperf"
      {"fixed", "<ios>", cxx98},
#line 184 "std-name-hint.gperf"
      {"weak_ptr", "<memory>", cxx11},
#line 104 "std-name-hint.gperf"
      {"setw", "<iomanip>", cxx98},
#line 152 "std-name-hint.gperf"
      {"front_inserter", "<iterator>", cxx98},
#line 221 "std-name-hint.gperf"
      {"basic_stringbuf", "<sstream>", cxx98},
#line 145 "std-name-hint.gperf"
      {"ws", "<istream>", cxx98},
#line 92 "std-name-hint.gperf"
      {"unwrap_ref_decay_t", "<functional>", cxx20},
#line 53 "std-name-hint.gperf"
      {"bitset", "<bitset>", cxx11},
#line 78 "std-name-hint.gperf"
      {"ifstream", "<fstream>", cxx98},
#line 138 "std-name-hint.gperf"
      {"cerr", "<iostream>", cxx98},
#line 88 "std-name-hint.gperf"
      {"reference_wrapper", "<functional>", cxx11},
#line 97 "std-name-hint.gperf"
      {"promise", "<future>", cxx11},
#line 161 "std-name-hint.gperf"
      {"prev", "<iterator>", cxx11},
#line 82 "std-name-hint.gperf"
      {"bind_front", "<functional>", cxx20},
#line 186 "std-name-hint.gperf"
      {"pmr", "<memory_resource>", cxx17},
#line 155 "std-name-hint.gperf"
      {"istreambuf_iterator", "<iterator>", cxx98},
#line 188 "std-name-hint.gperf"
      {"mutex", "<mutex>", cxx11},
#line 126 "std-name-hint.gperf"
      {"left", "<ios>", cxx98},
#line 128 "std-name-hint.gperf"
      {"dec", "<ios>", cxx98},
#line 81 "std-name-hint.gperf"
      {"bind", "<functional>", cxx11},
#line 120 "std-name-hint.gperf"
      {"noskipws", "<ios>", cxx98},
#line 167 "std-name-hint.gperf"
      {"endl", "<ostream>", cxx98},
#line 207 "std-name-hint.gperf"
      {"endl", "<ostream>", cxx98},
#line 130 "std-name-hint.gperf"
      {"oct", "<ios>", cxx98},
#line 137 "std-name-hint.gperf"
      {"cout", "<iostream>", cxx98},
#line 49 "std-name-hint.gperf"
      {"atomic", "<atomic>", cxx11},
#line 282 "std-name-hint.gperf"
      {"pair", "<utility>", cxx98},
#line 174 "std-name-hint.gperf"
      {"map", "<map>", cxx98},
#line 193 "std-name-hint.gperf"
      {"call_once", "<mutex>", cxx11},
#line 94 "std-name-hint.gperf"
      {"async", "<future>", cxx11},
#line 116 "std-name-hint.gperf"
      {"noshowpoint", "<ios>", cxx98},
#line 204 "std-name-hint.gperf"
      {"wostream", "<ostream>", cxx98},
#line 258 "std-name-hint.gperf"
      {"invoke_result", "<type_traits>", cxx17},
#line 118 "std-name-hint.gperf"
      {"noshowpos", "<ios>", cxx98},
#line 259 "std-name-hint.gperf"
      {"invoke_result_t", "<type_traits>", cxx17},
#line 241 "std-name-hint.gperf"
      {"thread", "<thread>", cxx11},
#line 103 "std-name-hint.gperf"
      {"setprecision", "<iomanip>", cxx98},
#line 113 "std-name-hint.gperf"
      {"showbase", "<ios>", cxx98},
#line 74 "std-name-hint.gperf"
      {"basic_ifstream", "<fstream>", cxx98},
#line 178 "std-name-hint.gperf"
      {"allocator", "<memory>", cxx98},
#line 133 "std-name-hint.gperf"
      {"hexfloat", "<ios>", cxx11},
#line 117 "std-name-hint.gperf"
      {"showpos", "<ios>", cxx98},
#line 170 "std-name-hint.gperf"
      {"flush_emit", "<ostream>", cxx20},
#line 250 "std-name-hint.gperf"
      {"tuple_cat", "<tuple>", cxx11},
#line 179 "std-name-hint.gperf"
      {"allocator_traits", "<memory>", cxx11},
#line 191 "std-name-hint.gperf"
      {"recursive_timed_mutex", "<mutex>", cxx11},
#line 108 "std-name-hint.gperf"
      {"put_time", "<iomanip>", cxx11},
#line 210 "std-name-hint.gperf"
      {"priority_queue", "<queue>", cxx98},
#line 190 "std-name-hint.gperf"
      {"recursive_mutex", "<mutex>", cxx11},
#line 232 "std-name-hint.gperf"
      {"string", "<string>", cxx98},
#line 107 "std-name-hint.gperf"
      {"get_time", "<iomanip>", cxx11},
#line 223 "std-name-hint.gperf"
      {"basic_ostringstream", "<sstream>", cxx98},
#line 73 "std-name-hint.gperf"
      {"basic_filebuf", "<fstream>", cxx98},
#line 272 "std-name-hint.gperf"
      {"unordered_map", "<unordered_map>", cxx11},
#line 121 "std-name-hint.gperf"
      {"uppercase", "<ios>", cxx98},
#line 273 "std-name-hint.gperf"
      {"unordered_multimap", "<unordered_map>", cxx11},
#line 182 "std-name-hint.gperf"
      {"shared_ptr", "<memory>", cxx11},
#line 42 "std-name-hint.gperf"
      {"any", "<any>", cxx17},
#line 175 "std-name-hint.gperf"
      {"multimap", "<map>", cxx98},
#line 46 "std-name-hint.gperf"
      {"array", "<array>", cxx11},
#line 136 "std-name-hint.gperf"
      {"cin", "<iostream>", cxx98},
#line 238 "std-name-hint.gperf"
      {"basic_string_view", "<string_view>", cxx17},
#line 168 "std-name-hint.gperf"
      {"emit_on_flush", "<ostream>", cxx20},
#line 180 "std-name-hint.gperf"
      {"make_shared", "<memory>", cxx11},
#line 44 "std-name-hint.gperf"
      {"make_any", "<any>", cxx17},
#line 172 "std-name-hint.gperf"
      {"list", "<list>", cxx98},
#line 226 "std-name-hint.gperf"
      {"ostringstream", "<sstream>", cxx98},
#line 47 "std-name-hint.gperf"
      {"to_array", "<array>", cxx20},
#line 150 "std-name-hint.gperf"
      {"distance", "<iterator>", cxx98},
#line 197 "std-name-hint.gperf"
      {"lock_guard", "<mutex>", cxx11},
#line 111 "std-name-hint.gperf"
      {"boolalpha", "<ios>", cxx98},
#line 59 "std-name-hint.gperf"
      {"strong_ordering", "<compare>", cxx20},
#line 196 "std-name-hint.gperf"
      {"try_lock", "<mutex>", cxx11},
#line 267 "std-name-hint.gperf"
      {"disjunction", "<type_traits>", cxx17},
#line 268 "std-name-hint.gperf"
      {"disjunction_v", "<type_traits>", cxx17},
#line 67 "std-name-hint.gperf"
      {"byte", "<cstddef>", cxx17},
#line 115 "std-name-hint.gperf"
      {"showpoint", "<ios>", cxx98},
#line 64 "std-name-hint.gperf"
      {"condition_variable", "<condition_variable>", cxx11},
#line 129 "std-name-hint.gperf"
      {"hex", "<ios>", cxx98},
#line 141 "std-name-hint.gperf"
      {"wcout", "<iostream>", cxx98},
#line 222 "std-name-hint.gperf"
      {"basic_istringstream", "<sstream>", cxx98},
#line 169 "std-name-hint.gperf"
      {"noemit_on_flush", "<ostream>", cxx20},
#line 125 "std-name-hint.gperf"
      {"internal", "<ios>", cxx98},
#line 140 "std-name-hint.gperf"
      {"wcin", "<iostream>", cxx98},
#line 234 "std-name-hint.gperf"
      {"u8string", "<string>", cxx20},
#line 56 "std-name-hint.gperf"
      {"strong_equality", "<compare>", cxx20},
#line 62 "std-name-hint.gperf"
      {"complex_literals", "<complex>", cxx14},
#line 194 "std-name-hint.gperf"
      {"lock", "<mutex>", cxx11},
#line 189 "std-name-hint.gperf"
      {"timed_mutex", "<mutex>", cxx11},
#line 231 "std-name-hint.gperf"
      {"basic_string", "<string>", cxx98},
#line 96 "std-name-hint.gperf"
      {"packaged_task", "<future>", cxx11},
#line 239 "std-name-hint.gperf"
      {"string_view", "<string_view>", cxx17},
#line 225 "std-name-hint.gperf"
      {"istringstream", "<sstream>", cxx98},
#line 198 "std-name-hint.gperf"
      {"unique_lock", "<mutex>", cxx11},
#line 263 "std-name-hint.gperf"
      {"type_identity_t", "<type_traits>", cxx20},
#line 216 "std-name-hint.gperf"
      {"shared_mutex", "<shared_mutex>", cxx17},
#line 265 "std-name-hint.gperf"
      {"conjunction", "<type_traits>", cxx17},
#line 266 "std-name-hint.gperf"
      {"conjunction_v", "<type_traits>", cxx17},
#line 217 "std-name-hint.gperf"
      {"shared_timed_mutex", "<shared_mutex>", cxx14},
#line 102 "std-name-hint.gperf"
      {"setfill", "<iomanip>", cxx98},
#line 236 "std-name-hint.gperf"
      {"u32string", "<string>", cxx11},
#line 235 "std-name-hint.gperf"
      {"u16string", "<string>", cxx11},
#line 278 "std-name-hint.gperf"
      {"declval", "<utility>", cxx11},
#line 91 "std-name-hint.gperf"
      {"unwrap_ref_decay", "<functional>", cxx20},
#line 201 "std-name-hint.gperf"
      {"make_optional", "<optional>", cxx17},
#line 200 "std-name-hint.gperf"
      {"optional", "<optional>", cxx17},
#line 84 "std-name-hint.gperf"
      {"hash", "<functional>", cxx11},
#line 166 "std-name-hint.gperf"
      {"flush", "<ostream>", cxx98},
#line 206 "std-name-hint.gperf"
      {"flush", "<ostream>", cxx98},
#line 244 "std-name-hint.gperf"
      {"apply", "<tuple>", cxx17},
#line 61 "std-name-hint.gperf"
      {"complex", "<complex>", cxx98},
#line 242 "std-name-hint.gperf"
      {"this_thread", "<thread>", cxx11},
#line 177 "std-name-hint.gperf"
      {"allocate_shared", "<memory>", cxx11},
#line 132 "std-name-hint.gperf"
      {"scientific", "<ios>", cxx98},
#line 192 "std-name-hint.gperf"
      {"once_flag", "<mutex>", cxx11},
#line 106 "std-name-hint.gperf"
      {"put_money", "<iomanip>", cxx11},
#line 105 "std-name-hint.gperf"
      {"get_money", "<iomanip>", cxx11},
#line 195 "std-name-hint.gperf"
      {"scoped_lock", "<mutex>", cxx17},
#line 58 "std-name-hint.gperf"
      {"weak_ordering", "<compare>", cxx20},
#line 55 "std-name-hint.gperf"
      {"weak_equality", "<compare>", cxx20},
#line 215 "std-name-hint.gperf"
      {"shared_lock", "<shared_mutex>", cxx14},
#line 50 "std-name-hint.gperf"
      {"atomic_flag", "<atomic>", cxx11},
#line 142 "std-name-hint.gperf"
      {"wclog", "<iostream>", cxx98},
#line 65 "std-name-hint.gperf"
      {"condition_variable_any", "<condition_variable>", cxx11},
#line 139 "std-name-hint.gperf"
      {"clog", "<iostream>", cxx98},
#line 262 "std-name-hint.gperf"
      {"type_identity", "<type_traits>", cxx20},
#line 233 "std-name-hint.gperf"
      {"wstring", "<string>", cxx98},
#line 57 "std-name-hint.gperf"
      {"partial_ordering", "<compare>", cxx20}
    };

  static const short lookup[] =
    {
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,    0,   -1,   -1,   -1,    1,   -1,    2,
        -1,    3,   -1, -228,   -1,    6, -201,   -2,
        -1,    7,    8,   -1,    9,   -1,   -1,   -1,
        -1,   -1,   -1,   10,   11,   -1,   -1,   12,
        -1,   13,   -1,   14,   15,   -1,   16,   -1,
        17,   -1,   -1,   18,   19,   20,   -1,   21,
        -1,   22,   -1,   -1,   23,   24, -287,   -1,
        27,   28,   29,   30,   31,   -1,   32,   -1,
        33,   34,   35,   36,   37,   -1,   38,   -1,
        39, -180,   -2,   -1,   -1,   -1,   40,   -1,
        41,   -1,   -1,   -1,   -1,   42,   43,   -1,
        44,   45,   46,   -1,   47,   -1,   48,   49,
        50,   51,   52,   53,   -1,   -1,   54,   -1,
        -1,   55,   56,   57,   58,   -1,   59,   -1,
        60,   61,   -1,   62,   -1,   63,   64,   -1,
        65,   -1,   -1,   -1,   66,   -1,   -1,   67,
        68,   69,   70,   -1,   -1,   71,   -1,   -1,
        72,   -1,   73,   -1,   -1,   74,   75,   -1,
        -1,   76,   -1,   77,   78,   79,   -1,   80,
        81,   -1,   -1,   -1,   -1,   82,   -1,   -1,
        83,   -1,   -1,   84,   -1,   85,   86,   87,
        -1,   88,   89,   -1,   90,   -1,   -1,   91,
        92,   93,   -1,   94,   95,   96,   -1,   97,
      -403,  100,  101,   -1,  102, -107,   -2,  103,
        -1,   -1,   -1,  104,  105,  106,  107,   -1,
        -1,   -1,   -1,  108,   -1,  109,  110,  111,
       112,  113,  114,  115,   -1,   -1,  116,  117,
        -1,   -1,   -1,  118,  119,  120,   -1,   -1,
        -1,   -1,  121,   -1,  122,   -1,  123,  124,
       125,  126,   -1,  127,  128,   -1,   -1,   -1,
       129,   -1,   -1,   -1,   -1,  130,  131,   -1,
        -1,   -1,  132,   -1,  133,   -1,  134,  135,
       136,   -1,   -1,   -1,  137,   -1,  138,   -1,
        -1,  139,   -1,   -1,  140,  141,   -1,   -1,
        -1,   -1,  142,  143,   -1,   -1,   -1,  144,
       145,   -1,   -1,   -1,   -1,  146,  147,  148,
        -1,   -1,  149,   -1,   -1,  150,  151,  152,
       153,   -1,   -1,  154,  155,   -1,   -1,   -1,
        -1,  156,  157,  158,  159,  160,  161,   -1,
        -1,   -1,   -1,   -1,  162,   -1,   -1,   -1,
        -1,   -1,   -1,  163,  164,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,  165,  166,  167,   -1,   -1,   -1,
       168,  169,   -1,   -1,  170,   -1,   -1,  171,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,  172,   -1,   -1,   -1,
       173,  174,  175,   -1,   -1,   -1,   -1,  176,
       177,   -1,   -1,   -1,   -1,  178,   -1,   -1,
        -1,  179,   -1,  180,   -1,   -1,   -1,   -1,
        -1,  181,   -1,   -1,   -1,   -1,  182,   -1,
        -1,  183,   -1,   -1,   -1, -620,  -21,   -2,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
       186,   -1,   -1,  187,   -1,   -1,   -1,  188,
        -1,  189,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,  190,   -1,  191,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,  192,
        -1,   -1,   -1,   -1,  193,   -1,   -1,  194,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,  195,   -1,   -1,   -1,  196,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
       197,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,  198,   -1,   -1,   -1,   -1,
        -1,   -1,  199,   -1,  200,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,  201,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,  202,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,  203,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
        -1,   -1,  204
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      unsigned int key = hash (str, len);

      if (key <= MAX_HASH_VALUE)
        {
          int index = lookup[key];

          if (index >= 0)
            {
              const char *s = wordlist[index].name;

              if (*str == *s && !strcmp (str + 1, s + 1))
                return &wordlist[index];
            }
          else if (index < -TOTAL_KEYWORDS)
            {
              int offset = - 1 - TOTAL_KEYWORDS - index;
              const struct std_name_hint *wordptr = &wordlist[TOTAL_KEYWORDS + lookup[offset]];
              const struct std_name_hint *wordendptr = wordptr + -lookup[offset + 1];

              while (wordptr < wordendptr)
                {
                  const char *s = wordptr->name;

                  if (*str == *s && !strcmp (str + 1, s + 1))
                    return wordptr;
                  wordptr++;
                }
            }
        }
    }
  return 0;
}
