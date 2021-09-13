/* C++ code produced by gperf version 3.1 */
/* Command-line: gperf -o -C -E -k '1-6,$' -j1 -D -N libc_name_p -L C++ --output-file cfns.h cfns.gperf  */

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

#line 4 "cfns.gperf"

/* Copyright (C) 2000-2021 Free Software Foundation, Inc.

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
#line 23 "cfns.gperf"
struct libc_name_struct { const char *name; int c_ver; };
/* maximum key range = 1478, duplicates = 0 */

class libc_name
{
private:
  static inline unsigned int hash (const char *str, size_t len);
public:
  static const struct libc_name_struct *libc_name_p (const char *str, size_t len);
};

inline unsigned int
libc_name::hash (const char *str, size_t len)
{
  static const unsigned short asso_values[] =
    {
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,    0,    1,
        82, 1488,    4, 1488,    1, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
         0, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488,    0,    0,   29,    7,    6,
       228,  136,    0,  284,  232,   17,  447,   31,    2,   51,
         0,   78,  358,  433,   31,    0,   17,   52,  356,  230,
       377,    5,   89, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488, 1488,
      1488, 1488, 1488, 1488, 1488, 1488, 1488
    };
  unsigned int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[static_cast<unsigned char>(str[5]+1)];
      /*FALLTHROUGH*/
      case 5:
        hval += asso_values[static_cast<unsigned char>(str[4])];
      /*FALLTHROUGH*/
      case 4:
        hval += asso_values[static_cast<unsigned char>(str[3])];
      /*FALLTHROUGH*/
      case 3:
        hval += asso_values[static_cast<unsigned char>(str[2])];
      /*FALLTHROUGH*/
      case 2:
        hval += asso_values[static_cast<unsigned char>(str[1]+1)];
      /*FALLTHROUGH*/
      case 1:
        hval += asso_values[static_cast<unsigned char>(str[0])];
        break;
    }
  return hval + asso_values[static_cast<unsigned char>(str[len - 1])];
}

const struct libc_name_struct *
libc_name::libc_name_p (const char *str, size_t len)
{
  enum
    {
      TOTAL_KEYWORDS = 414,
      MIN_WORD_LENGTH = 3,
      MAX_WORD_LENGTH = 15,
      MIN_HASH_VALUE = 10,
      MAX_HASH_VALUE = 1487
    };

  static const struct libc_name_struct wordlist[] =
    {
#line 456 "cfns.gperf"
      {"nan", 99},
#line 457 "cfns.gperf"
      {"nanf", 99},
#line 458 "cfns.gperf"
      {"nanl", 99},
#line 59 "cfns.gperf"
      {"fabs", 89},
#line 396 "cfns.gperf"
      {"fabsf", 99},
#line 119 "cfns.gperf"
      {"labs", 89},
#line 471 "cfns.gperf"
      {"fmin", 99},
#line 472 "cfns.gperf"
      {"fminf", 99},
#line 397 "cfns.gperf"
      {"fabsl", 99},
#line 286 "cfns.gperf"
      {"cabs", 99},
#line 287 "cfns.gperf"
      {"cabsf", 99},
#line 473 "cfns.gperf"
      {"fminl", 99},
#line 192 "cfns.gperf"
      {"tan", 89},
#line 340 "cfns.gperf"
      {"tanf", 99},
#line 288 "cfns.gperf"
      {"cabsl", 99},
#line 417 "cfns.gperf"
      {"ceilf", 99},
#line 49 "cfns.gperf"
      {"ceil", 89},
#line 341 "cfns.gperf"
      {"tanl", 99},
#line 475 "cfns.gperf"
      {"fmaf", 99},
#line 418 "cfns.gperf"
      {"ceill", 99},
#line 247 "cfns.gperf"
      {"casin", 99},
#line 476 "cfns.gperf"
      {"fmal", 99},
#line 36 "cfns.gperf"
      {"abs", 89},
#line 256 "cfns.gperf"
      {"csin", 99},
#line 257 "cfns.gperf"
      {"csinf", 99},
#line 258 "cfns.gperf"
      {"csinl", 99},
#line 266 "cfns.gperf"
      {"casinhf", 99},
#line 267 "cfns.gperf"
      {"casinhl", 99},
#line 427 "cfns.gperf"
      {"lrint", 99},
#line 474 "cfns.gperf"
      {"fma", 99},
#line 250 "cfns.gperf"
      {"catan", 99},
#line 394 "cfns.gperf"
      {"cbrtf", 99},
#line 98 "cfns.gperf"
      {"iscntrl", 89},
#line 39 "cfns.gperf"
      {"asin", 89},
#line 330 "cfns.gperf"
      {"asinf", 99},
#line 395 "cfns.gperf"
      {"cbrtl", 99},
#line 331 "cfns.gperf"
      {"asinl", 99},
#line 393 "cfns.gperf"
      {"cbrt", 99},
#line 61 "cfns.gperf"
      {"feof", 89},
#line 269 "cfns.gperf"
      {"catanhf", 99},
#line 403 "cfns.gperf"
      {"sqrtf", 99},
#line 270 "cfns.gperf"
      {"catanhl", 99},
#line 442 "cfns.gperf"
      {"trunc", 99},
#line 404 "cfns.gperf"
      {"sqrtl", 99},
#line 249 "cfns.gperf"
      {"casinl", 99},
#line 259 "cfns.gperf"
      {"ctan", 99},
#line 260 "cfns.gperf"
      {"ctanf", 99},
#line 488 "cfns.gperf"
      {"llabs", 99},
#line 429 "cfns.gperf"
      {"lrintl", 99},
#line 261 "cfns.gperf"
      {"ctanl", 99},
#line 165 "cfns.gperf"
      {"sqrt", 89},
#line 244 "cfns.gperf"
      {"cacos", 99},
#line 40 "cfns.gperf"
      {"atan", 89},
#line 332 "cfns.gperf"
      {"atanf", 99},
#line 252 "cfns.gperf"
      {"catanl", 99},
#line 333 "cfns.gperf"
      {"atanl", 99},
#line 177 "cfns.gperf"
      {"strncat", 89},
#line 263 "cfns.gperf"
      {"cacoshf", 99},
#line 264 "cfns.gperf"
      {"cacoshl", 99},
#line 313 "cfns.gperf"
      {"feraiseexcept", 99},
#line 444 "cfns.gperf"
      {"truncl", 99},
#line 183 "cfns.gperf"
      {"strstr", 89},
#line 405 "cfns.gperf"
      {"erf", 99},
#line 406 "cfns.gperf"
      {"erff", 99},
#line 407 "cfns.gperf"
      {"erfl", 99},
#line 409 "cfns.gperf"
      {"erfcf", 99},
#line 410 "cfns.gperf"
      {"erfcl", 99},
#line 408 "cfns.gperf"
      {"erfc", 99},
#line 246 "cfns.gperf"
      {"cacosl", 99},
#line 431 "cfns.gperf"
      {"llrintf", 99},
#line 432 "cfns.gperf"
      {"llrintl", 99},
#line 43 "cfns.gperf"
      {"atof", 89},
#line 422 "cfns.gperf"
      {"nearbyintf", 99},
#line 45 "cfns.gperf"
      {"atol", 89},
#line 423 "cfns.gperf"
      {"nearbyintl", 99},
#line 482 "cfns.gperf"
      {"atoll", 99},
#line 181 "cfns.gperf"
      {"strrchr", 89},
#line 430 "cfns.gperf"
      {"llrint", 99},
#line 62 "cfns.gperf"
      {"ferror", 89},
#line 307 "cfns.gperf"
      {"creal", 99},
#line 311 "cfns.gperf"
      {"feclearexcept", 99},
#line 421 "cfns.gperf"
      {"nearbyint", 99},
#line 310 "cfns.gperf"
      {"isblank", 99},
#line 168 "cfns.gperf"
      {"strcat", 89},
#line 57 "cfns.gperf"
      {"exit", 89},
#line 44 "cfns.gperf"
      {"atoi", 89},
#line 128 "cfns.gperf"
      {"mblen", 89},
#line 51 "cfns.gperf"
      {"clock", 89},
#line 466 "cfns.gperf"
      {"fdimf", 99},
#line 467 "cfns.gperf"
      {"fdiml", 99},
#line 448 "cfns.gperf"
      {"remainderf", 99},
#line 185 "cfns.gperf"
      {"strtok", 89},
#line 449 "cfns.gperf"
      {"remainderl", 99},
#line 171 "cfns.gperf"
      {"strcoll", 89},
#line 38 "cfns.gperf"
      {"asctime", 89},
#line 309 "cfns.gperf"
      {"creall", 99},
#line 315 "cfns.gperf"
      {"fetestexcept", 99},
#line 186 "cfns.gperf"
      {"strtol", 89},
#line 485 "cfns.gperf"
      {"strtoll", 99},
#line 447 "cfns.gperf"
      {"remainder", 99},
#line 335 "cfns.gperf"
      {"atan2l", 99},
#line 465 "cfns.gperf"
      {"fdim", 99},
#line 355 "cfns.gperf"
      {"tanhf", 99},
#line 356 "cfns.gperf"
      {"tanhl", 99},
#line 69 "cfns.gperf"
      {"floor", 89},
#line 437 "cfns.gperf"
      {"lroundf", 99},
#line 438 "cfns.gperf"
      {"lroundl", 99},
#line 41 "cfns.gperf"
      {"atan2", 89},
#line 140 "cfns.gperf"
      {"mktime", 89},
#line 265 "cfns.gperf"
      {"casinh", 99},
#line 50 "cfns.gperf"
      {"clearerr", 89},
#line 420 "cfns.gperf"
      {"floorl", 99},
#line 440 "cfns.gperf"
      {"llroundf", 99},
#line 441 "cfns.gperf"
      {"llroundl", 99},
#line 176 "cfns.gperf"
      {"strlen", 89},
#line 129 "cfns.gperf"
      {"mbrlen", 89},
#line 445 "cfns.gperf"
      {"fmodf", 99},
#line 139 "cfns.gperf"
      {"memset", 89},
#line 268 "cfns.gperf"
      {"catanh", 99},
#line 446 "cfns.gperf"
      {"fmodl", 99},
#line 253 "cfns.gperf"
      {"ccos", 99},
#line 254 "cfns.gperf"
      {"ccosf", 99},
#line 497 "cfns.gperf"
      {"iswblank", 99},
#line 248 "cfns.gperf"
      {"casinf", 99},
#line 255 "cfns.gperf"
      {"ccosl", 99},
#line 390 "cfns.gperf"
      {"scalbln", 99},
#line 391 "cfns.gperf"
      {"scalblnf", 99},
#line 428 "cfns.gperf"
      {"lrintf", 99},
#line 392 "cfns.gperf"
      {"scalblnl", 99},
#line 276 "cfns.gperf"
      {"csinhl", 99},
#line 108 "cfns.gperf"
      {"iswcntrl", 89},
#line 296 "cfns.gperf"
      {"cargf", 99},
#line 48 "cfns.gperf"
      {"calloc", 89},
#line 297 "cfns.gperf"
      {"cargl", 99},
#line 37 "cfns.gperf"
      {"acos", 89},
#line 328 "cfns.gperf"
      {"acosf", 99},
#line 167 "cfns.gperf"
      {"sscanf", 89},
#line 329 "cfns.gperf"
      {"acosl", 99},
#line 251 "cfns.gperf"
      {"catanf", 99},
#line 387 "cfns.gperf"
      {"scalbn", 99},
#line 388 "cfns.gperf"
      {"scalbnf", 99},
#line 262 "cfns.gperf"
      {"cacosh", 99},
#line 389 "cfns.gperf"
      {"scalbnl", 99},
#line 347 "cfns.gperf"
      {"asinhl", 99},
#line 82 "cfns.gperf"
      {"fseek", 89},
#line 169 "cfns.gperf"
      {"strchr", 89},
#line 443 "cfns.gperf"
      {"truncf", 99},
#line 158 "cfns.gperf"
      {"setbuf", 89},
#line 52 "cfns.gperf"
      {"cos", 89},
#line 336 "cfns.gperf"
      {"cosf", 99},
#line 337 "cfns.gperf"
      {"cosl", 99},
#line 135 "cfns.gperf"
      {"memchr", 89},
#line 279 "cfns.gperf"
      {"ctanhl", 99},
#line 412 "cfns.gperf"
      {"lgammaf", 99},
#line 127 "cfns.gperf"
      {"malloc", 89},
#line 413 "cfns.gperf"
      {"lgammal", 99},
#line 101 "cfns.gperf"
      {"islower", 89},
#line 47 "cfns.gperf"
      {"btowc", 89},
#line 245 "cfns.gperf"
      {"cacosf", 99},
#line 317 "cfns.gperf"
      {"fesetround", 99},
#line 415 "cfns.gperf"
      {"tgammaf", 99},
#line 416 "cfns.gperf"
      {"tgammal", 99},
#line 504 "cfns.gperf"
      {"aligned_alloc", 11},
#line 350 "cfns.gperf"
      {"atanhl", 99},
#line 54 "cfns.gperf"
      {"ctime", 89},
#line 411 "cfns.gperf"
      {"lgamma", 99},
#line 469 "cfns.gperf"
      {"fmaxf", 99},
#line 78 "cfns.gperf"
      {"free", 89},
#line 470 "cfns.gperf"
      {"fmaxl", 99},
#line 533 "cfns.gperf"
      {"mbrtoc16", 11},
#line 414 "cfns.gperf"
      {"tgamma", 99},
#line 284 "cfns.gperf"
      {"clogf", 99},
#line 285 "cfns.gperf"
      {"clogl", 99},
#line 106 "cfns.gperf"
      {"iswalnum", 89},
#line 487 "cfns.gperf"
      {"_Exit", 99},
#line 153 "cfns.gperf"
      {"realloc", 89},
#line 241 "cfns.gperf"
      {"wmemset", 89},
#line 367 "cfns.gperf"
      {"ilogb", 99},
#line 162 "cfns.gperf"
      {"sin", 89},
#line 338 "cfns.gperf"
      {"sinf", 99},
#line 314 "cfns.gperf"
      {"fesetexceptflag", 99},
#line 178 "cfns.gperf"
      {"strncmp", 89},
#line 339 "cfns.gperf"
      {"sinl", 99},
#line 308 "cfns.gperf"
      {"crealf", 99},
#line 484 "cfns.gperf"
      {"strtold", 99},
#line 322 "cfns.gperf"
      {"imaxabs", 99},
#line 483 "cfns.gperf"
      {"strtof", 99},
#line 159 "cfns.gperf"
      {"setlocale", 89},
#line 132 "cfns.gperf"
      {"mbsrtowcs", 89},
#line 97 "cfns.gperf"
      {"isalpha", 89},
#line 319 "cfns.gperf"
      {"feholdexcept", 99},
#line 237 "cfns.gperf"
      {"wmemchr", 89},
#line 96 "cfns.gperf"
      {"isalnum", 89},
#line 334 "cfns.gperf"
      {"atan2f", 99},
#line 180 "cfns.gperf"
      {"strpbrk", 89},
#line 166 "cfns.gperf"
      {"srand", 89},
#line 193 "cfns.gperf"
      {"tanh", 89},
#line 481 "cfns.gperf"
      {"vsscanf", 99},
#line 219 "cfns.gperf"
      {"wcsncat", 89},
#line 369 "cfns.gperf"
      {"ilogbl", 99},
#line 190 "cfns.gperf"
      {"swscanf", 89},
#line 152 "cfns.gperf"
      {"rand", 89},
#line 425 "cfns.gperf"
      {"rintf", 99},
#line 535 "cfns.gperf"
      {"mbrtoc32", 11},
#line 436 "cfns.gperf"
      {"lround", 99},
#line 357 "cfns.gperf"
      {"expf", 99},
#line 426 "cfns.gperf"
      {"rintl", 99},
#line 358 "cfns.gperf"
      {"expl", 99},
#line 274 "cfns.gperf"
      {"csinh", 99},
#line 534 "cfns.gperf"
      {"c16rtomb", 11},
#line 95 "cfns.gperf"
      {"gmtime", 89},
#line 226 "cfns.gperf"
      {"wcsstr", 89},
#line 536 "cfns.gperf"
      {"c32rtomb", 11},
#line 424 "cfns.gperf"
      {"rint", 99},
#line 320 "cfns.gperf"
      {"fesetenv", 99},
#line 188 "cfns.gperf"
      {"strxfrm", 89},
#line 109 "cfns.gperf"
      {"iswctype", 89},
#line 182 "cfns.gperf"
      {"strspn", 89},
#line 292 "cfns.gperf"
      {"csqrt", 99},
#line 439 "cfns.gperf"
      {"llround", 99},
#line 419 "cfns.gperf"
      {"floorf", 99},
#line 173 "cfns.gperf"
      {"strcspn", 89},
#line 345 "cfns.gperf"
      {"asinh", 99},
#line 179 "cfns.gperf"
      {"strncpy", 89},
#line 133 "cfns.gperf"
      {"mbstowcs", 89},
#line 70 "cfns.gperf"
      {"fmod", 89},
#line 164 "cfns.gperf"
      {"sprintf", 89},
#line 102 "cfns.gperf"
      {"isprint", 89},
#line 187 "cfns.gperf"
      {"strtoul", 89},
#line 486 "cfns.gperf"
      {"strtoull", 99},
#line 131 "cfns.gperf"
      {"mbsinit", 89},
#line 172 "cfns.gperf"
      {"strcpy", 89},
#line 184 "cfns.gperf"
      {"strtod", 89},
#line 223 "cfns.gperf"
      {"wcsrchr", 89},
#line 277 "cfns.gperf"
      {"ctanh", 99},
#line 362 "cfns.gperf"
      {"expm1", 99},
#line 230 "cfns.gperf"
      {"wcstombs", 89},
#line 275 "cfns.gperf"
      {"csinhf", 99},
#line 294 "cfns.gperf"
      {"csqrtl", 99},
#line 137 "cfns.gperf"
      {"memcpy", 89},
#line 211 "cfns.gperf"
      {"wcscat", 89},
#line 477 "cfns.gperf"
      {"snprintf", 99},
#line 233 "cfns.gperf"
      {"wctob", 89},
#line 130 "cfns.gperf"
      {"mbrtowc", 89},
#line 348 "cfns.gperf"
      {"atanh", 99},
#line 346 "cfns.gperf"
      {"asinhf", 99},
#line 360 "cfns.gperf"
      {"exp2f", 99},
#line 361 "cfns.gperf"
      {"exp2l", 99},
#line 228 "cfns.gperf"
      {"wcstok", 89},
#line 123 "cfns.gperf"
      {"localtime", 89},
#line 210 "cfns.gperf"
      {"wcrtomb", 89},
#line 351 "cfns.gperf"
      {"coshf", 99},
#line 214 "cfns.gperf"
      {"wcscoll", 89},
#line 273 "cfns.gperf"
      {"ccoshl", 99},
#line 138 "cfns.gperf"
      {"memmove", 89},
#line 352 "cfns.gperf"
      {"coshl", 99},
#line 278 "cfns.gperf"
      {"ctanhf", 99},
#line 364 "cfns.gperf"
      {"expm1l", 99},
#line 229 "cfns.gperf"
      {"wcstol", 89},
#line 495 "cfns.gperf"
      {"wcstoll", 99},
#line 295 "cfns.gperf"
      {"carg", 99},
#line 235 "cfns.gperf"
      {"wctrans", 89},
#line 134 "cfns.gperf"
      {"mbtowc", 89},
#line 234 "cfns.gperf"
      {"wctomb", 89},
#line 491 "cfns.gperf"
      {"vswscanf", 99},
#line 344 "cfns.gperf"
      {"acoshl", 99},
#line 349 "cfns.gperf"
      {"atanhf", 99},
#line 199 "cfns.gperf"
      {"towctrans", 89},
#line 141 "cfns.gperf"
      {"modf", 89},
#line 385 "cfns.gperf"
      {"modff", 99},
#line 386 "cfns.gperf"
      {"modfl", 99},
#line 372 "cfns.gperf"
      {"logf", 99},
#line 125 "cfns.gperf"
      {"log10", 89},
#line 373 "cfns.gperf"
      {"logl", 99},
#line 383 "cfns.gperf"
      {"logbf", 99},
#line 384 "cfns.gperf"
      {"logbl", 99},
#line 382 "cfns.gperf"
      {"logb", 99},
#line 359 "cfns.gperf"
      {"exp2", 99},
#line 316 "cfns.gperf"
      {"fegetround", 99},
#line 218 "cfns.gperf"
      {"wcslen", 89},
#line 353 "cfns.gperf"
      {"sinhf", 99},
#line 42 "cfns.gperf"
      {"atexit", 89},
#line 354 "cfns.gperf"
      {"sinhl", 99},
#line 175 "cfns.gperf"
      {"strftime", 89},
#line 103 "cfns.gperf"
      {"ispunct", 89},
#line 375 "cfns.gperf"
      {"log10l", 99},
#line 283 "cfns.gperf"
      {"clog", 99},
#line 460 "cfns.gperf"
      {"nextafterf", 99},
#line 461 "cfns.gperf"
      {"nextafterl", 99},
#line 197 "cfns.gperf"
      {"tolower", 89},
#line 368 "cfns.gperf"
      {"ilogbf", 99},
#line 435 "cfns.gperf"
      {"roundl", 99},
#line 380 "cfns.gperf"
      {"log2f", 99},
#line 212 "cfns.gperf"
      {"wcschr", 89},
#line 381 "cfns.gperf"
      {"log2l", 99},
#line 312 "cfns.gperf"
      {"fegetexceptflag", 99},
#line 160 "cfns.gperf"
      {"setvbuf", 89},
#line 281 "cfns.gperf"
      {"cexpf", 99},
#line 459 "cfns.gperf"
      {"nextafter", 99},
#line 282 "cfns.gperf"
      {"cexpl", 99},
#line 290 "cfns.gperf"
      {"cpowf", 99},
#line 291 "cfns.gperf"
      {"cpowl", 99},
#line 112 "cfns.gperf"
      {"iswlower", 89},
#line 122 "cfns.gperf"
      {"localeconv", 89},
#line 107 "cfns.gperf"
      {"iswalpha", 89},
#line 110 "cfns.gperf"
      {"iswdigit", 89},
#line 115 "cfns.gperf"
      {"iswspace", 89},
#line 114 "cfns.gperf"
      {"iswpunct", 89},
#line 463 "cfns.gperf"
      {"nexttowardf", 99},
#line 271 "cfns.gperf"
      {"ccosh", 99},
#line 464 "cfns.gperf"
      {"nexttowardl", 99},
#line 468 "cfns.gperf"
      {"fmax", 99},
#line 238 "cfns.gperf"
      {"wmemcmp", 89},
#line 194 "cfns.gperf"
      {"time", 89},
#line 104 "cfns.gperf"
      {"isspace", 89},
#line 293 "cfns.gperf"
      {"csqrtf", 99},
#line 398 "cfns.gperf"
      {"hypot", 99},
#line 318 "cfns.gperf"
      {"fegetenv", 99},
#line 342 "cfns.gperf"
      {"acosh", 99},
#line 379 "cfns.gperf"
      {"log2", 99},
#line 302 "cfns.gperf"
      {"conjf", 99},
#line 161 "cfns.gperf"
      {"signal", 89},
#line 303 "cfns.gperf"
      {"conjl", 99},
#line 111 "cfns.gperf"
      {"iswgraph", 89},
#line 220 "cfns.gperf"
      {"wcsncmp", 89},
#line 53 "cfns.gperf"
      {"cosh", 89},
#line 400 "cfns.gperf"
      {"hypotl", 99},
#line 272 "cfns.gperf"
      {"ccoshf", 99},
#line 494 "cfns.gperf"
      {"wcstold", 99},
#line 105 "cfns.gperf"
      {"isupper", 89},
#line 363 "cfns.gperf"
      {"expm1f", 99},
#line 493 "cfns.gperf"
      {"wcstof", 99},
#line 343 "cfns.gperf"
      {"acoshf", 99},
#line 58 "cfns.gperf"
      {"exp", 89},
#line 222 "cfns.gperf"
      {"wcspbrk", 89},
#line 207 "cfns.gperf"
      {"vsprintf", 89},
#line 239 "cfns.gperf"
      {"wmemcpy", 89},
#line 189 "cfns.gperf"
      {"swprintf", 89},
#line 506 "cfns.gperf"
      {"quick_exit", 11},
#line 121 "cfns.gperf"
      {"ldiv", 89},
#line 224 "cfns.gperf"
      {"wcsrtombs", 89},
#line 300 "cfns.gperf"
      {"cimagl", 99},
#line 232 "cfns.gperf"
      {"wcsxfrm", 89},
#line 85 "cfns.gperf"
      {"fwide", 89},
#line 225 "cfns.gperf"
      {"wcsspn", 89},
#line 433 "cfns.gperf"
      {"round", 99},
#line 216 "cfns.gperf"
      {"wcscspn", 89},
#line 221 "cfns.gperf"
      {"wcsncpy", 89},
#line 118 "cfns.gperf"
      {"isxdigit", 89},
#line 163 "cfns.gperf"
      {"sinh", 89},
#line 231 "cfns.gperf"
      {"wcstoul", 89},
#line 496 "cfns.gperf"
      {"wcstoull", 99},
#line 325 "cfns.gperf"
      {"strtoumax", 99},
#line 215 "cfns.gperf"
      {"wcscpy", 89},
#line 227 "cfns.gperf"
      {"wcstod", 89},
#line 366 "cfns.gperf"
      {"frexpl", 99},
#line 124 "cfns.gperf"
      {"log", 89},
#line 451 "cfns.gperf"
      {"remquof", 99},
#line 452 "cfns.gperf"
      {"remquol", 99},
#line 374 "cfns.gperf"
      {"log10f", 99},
#line 170 "cfns.gperf"
      {"strcmp", 89},
#line 401 "cfns.gperf"
      {"powf", 99},
#line 402 "cfns.gperf"
      {"powl", 99},
#line 136 "cfns.gperf"
      {"memcmp", 89},
#line 434 "cfns.gperf"
      {"roundf", 99},
#line 240 "cfns.gperf"
      {"wmemmove", 89},
#line 236 "cfns.gperf"
      {"wctype", 89},
#line 289 "cfns.gperf"
      {"cpow", 99},
#line 462 "cfns.gperf"
      {"nexttoward", 99},
#line 450 "cfns.gperf"
      {"remquo", 99},
#line 324 "cfns.gperf"
      {"strtoimax", 99},
#line 321 "cfns.gperf"
      {"feupdateenv", 99},
#line 489 "cfns.gperf"
      {"lldiv", 99},
#line 91 "cfns.gperf"
      {"getenv", 89},
#line 99 "cfns.gperf"
      {"isdigit", 89},
#line 505 "cfns.gperf"
      {"at_quick_exit", 11},
#line 100 "cfns.gperf"
      {"isgraph", 89},
#line 306 "cfns.gperf"
      {"cprojl", 99},
#line 378 "cfns.gperf"
      {"log1pl", 99},
#line 399 "cfns.gperf"
      {"hypotf", 99},
#line 217 "cfns.gperf"
      {"wcsftime", 89},
#line 371 "cfns.gperf"
      {"ldexpl", 99},
#line 200 "cfns.gperf"
      {"towlower", 89},
#line 280 "cfns.gperf"
      {"cexp", 99},
#line 298 "cfns.gperf"
      {"cimag", 99},
#line 299 "cfns.gperf"
      {"cimagf", 99},
#line 532 "cfns.gperf"
      {"timespec_get", 11},
#line 113 "cfns.gperf"
      {"iswprint", 89},
#line 116 "cfns.gperf"
      {"iswupper", 89},
#line 365 "cfns.gperf"
      {"frexpf", 99},
#line 143 "cfns.gperf"
      {"pow", 89},
#line 198 "cfns.gperf"
      {"toupper", 89},
#line 453 "cfns.gperf"
      {"copysign", 99},
#line 454 "cfns.gperf"
      {"copysignf", 99},
#line 455 "cfns.gperf"
      {"copysignl", 99},
#line 480 "cfns.gperf"
      {"vsnprintf", 99},
#line 80 "cfns.gperf"
      {"frexp", 89},
#line 301 "cfns.gperf"
      {"conj", 99},
#line 55 "cfns.gperf"
      {"difftime", 89},
#line 305 "cfns.gperf"
      {"cprojf", 99},
#line 377 "cfns.gperf"
      {"log1pf", 99},
#line 327 "cfns.gperf"
      {"wcstoumax", 99},
#line 370 "cfns.gperf"
      {"ldexpf", 99},
#line 213 "cfns.gperf"
      {"wcscmp", 89},
#line 117 "cfns.gperf"
      {"iswxdigit", 89},
#line 376 "cfns.gperf"
      {"log1p", 99},
#line 120 "cfns.gperf"
      {"ldexp", 89},
#line 326 "cfns.gperf"
      {"wcstoimax", 99},
#line 56 "cfns.gperf"
      {"div", 89},
#line 304 "cfns.gperf"
      {"cproj", 99},
#line 208 "cfns.gperf"
      {"vswprintf", 89},
#line 126 "cfns.gperf"
      {"longjmp", 89},
#line 323 "cfns.gperf"
      {"imaxdiv", 99},
#line 201 "cfns.gperf"
      {"towupper", 89}
    };

  static const short lookup[] =
    {
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
        0,   1,  -1,  -1,  -1,   2,  -1,  -1,   3,   4,
        5,   6,   7,   8,   9,  10,  11,  12,  13,  14,
       15,  16,  17,  18,  19,  20,  -1,  21,  22,  -1,
       -1,  -1,  -1,  -1,  23,  24,  -1,  -1,  -1,  25,
       -1,  -1,  -1,  -1,  26,  -1,  27,  -1,  28,  -1,
       -1,  29,  -1,  -1,  30,  31,  32,  33,  34,  35,
       -1,  -1,  36,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  37,  38,  39,  40,  41,  42,  -1,  43,  44,
       -1,  45,  46,  -1,  47,  48,  49,  -1,  -1,  -1,
       50,  -1,  51,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  52,  53,  -1,  -1,  54,  55,
       56,  57,  -1,  58,  59,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  60,  -1,  -1,  61,  -1,  62,
       63,  -1,  -1,  -1,  64,  -1,  -1,  65,  -1,  -1,
       -1,  66,  67,  -1,  -1,  -1,  68,  -1,  -1,  -1,
       69,  -1,  70,  71,  -1,  -1,  72,  73,  74,  -1,
       75,  -1,  -1,  -1,  -1,  76,  77,  78,  -1,  -1,
       79,  80,  81,  -1,  -1,  -1,  -1,  -1,  82,  -1,
       -1,  -1,  -1,  83,  -1,  -1,  84,  85,  -1,  -1,
       86,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  87,  88,
       -1,  -1,  -1,  89,  -1,  -1,  90,  91,  92,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  93,  -1,  94,
       -1,  -1,  95,  -1,  96,  -1,  -1,  97,  98,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  99,  -1,  -1,  -1,
       -1, 100,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 101,
       -1, 102,  -1,  -1,  -1, 103,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1, 104, 105,  -1, 106,  -1, 107,
      108,  -1,  -1,  -1,  -1, 109,  -1,  -1,  -1,  -1,
       -1,  -1, 110,  -1,  -1,  -1,  -1, 111,  -1,  -1,
      112,  -1, 113,  -1,  -1, 114,  -1,  -1,  -1,  -1,
      115, 116,  -1, 117, 118, 119, 120, 121,  -1, 122,
      123, 124,  -1,  -1, 125, 126, 127, 128,  -1,  -1,
       -1, 129, 130, 131,  -1, 132,  -1, 133,  -1, 134,
      135,  -1, 136,  -1, 137,  -1,  -1,  -1,  -1, 138,
      139, 140, 141, 142, 143,  -1, 144,  -1, 145,  -1,
       -1,  -1,  -1,  -1,  -1, 146, 147, 148, 149,  -1,
       -1,  -1, 150,  -1,  -1,  -1,  -1, 151, 152, 153,
      154, 155, 156,  -1, 157,  -1,  -1, 158,  -1,  -1,
       -1, 159,  -1,  -1, 160,  -1, 161,  -1,  -1,  -1,
      162, 163,  -1, 164,  -1,  -1,  -1, 165,  -1,  -1,
       -1, 166, 167,  -1,  -1, 168,  -1,  -1,  -1,  -1,
      169,  -1, 170,  -1, 171,  -1,  -1,  -1, 172,  -1,
       -1,  -1, 173, 174,  -1, 175,  -1,  -1,  -1,  -1,
       -1, 176,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 177,
      178, 179, 180,  -1, 181, 182,  -1,  -1,  -1,  -1,
       -1,  -1,  -1, 183, 184, 185,  -1,  -1, 186,  -1,
      187,  -1, 188,  -1,  -1,  -1, 189, 190, 191, 192,
       -1,  -1, 193,  -1,  -1,  -1, 194,  -1,  -1,  -1,
      195,  -1, 196, 197,  -1, 198, 199, 200, 201,  -1,
      202, 203, 204, 205, 206,  -1,  -1, 207,  -1, 208,
      209, 210, 211, 212,  -1,  -1, 213, 214, 215,  -1,
      216,  -1,  -1,  -1,  -1, 217, 218, 219, 220, 221,
       -1,  -1, 222,  -1, 223,  -1,  -1, 224, 225,  -1,
      226,  -1, 227, 228, 229, 230,  -1, 231, 232,  -1,
      233,  -1,  -1,  -1,  -1,  -1, 234, 235,  -1,  -1,
       -1, 236, 237, 238,  -1,  -1, 239,  -1, 240,  -1,
      241,  -1, 242, 243,  -1,  -1,  -1,  -1,  -1, 244,
       -1,  -1,  -1,  -1,  -1, 245, 246,  -1,  -1,  -1,
      247,  -1, 248,  -1, 249,  -1,  -1,  -1, 250,  -1,
       -1, 251, 252, 253, 254, 255,  -1,  -1,  -1, 256,
      257,  -1, 258, 259,  -1,  -1, 260,  -1,  -1,  -1,
      261,  -1, 262, 263, 264,  -1, 265,  -1,  -1,  -1,
       -1,  -1, 266,  -1,  -1,  -1,  -1, 267,  -1,  -1,
       -1, 268, 269,  -1,  -1,  -1, 270,  -1, 271,  -1,
      272,  -1, 273,  -1,  -1,  -1, 274,  -1,  -1,  -1,
      275,  -1, 276,  -1,  -1,  -1,  -1, 277,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 278,  -1,  -1,  -1,  -1,
      279,  -1,  -1,  -1, 280,  -1, 281,  -1, 282,  -1,
       -1, 283,  -1,  -1,  -1,  -1, 284,  -1,  -1,  -1,
       -1,  -1,  -1,  -1, 285,  -1,  -1, 286,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1, 287,  -1, 288,
       -1,  -1,  -1, 289,  -1,  -1,  -1, 290, 291,  -1,
       -1, 292,  -1, 293,  -1, 294, 295,  -1,  -1,  -1,
       -1,  -1,  -1, 296,  -1,  -1, 297, 298,  -1,  -1,
      299,  -1, 300,  -1,  -1,  -1, 301,  -1,  -1,  -1,
      302,  -1,  -1, 303,  -1, 304, 305,  -1,  -1,  -1,
       -1,  -1,  -1, 306,  -1,  -1,  -1, 307,  -1,  -1,
      308, 309, 310,  -1,  -1,  -1,  -1, 311, 312,  -1,
       -1, 313, 314,  -1, 315,  -1, 316,  -1,  -1,  -1,
       -1, 317,  -1,  -1, 318,  -1,  -1,  -1,  -1,  -1,
       -1,  -1, 319,  -1,  -1,  -1, 320,  -1,  -1, 321,
      322,  -1,  -1,  -1,  -1,  -1, 323,  -1,  -1, 324,
       -1,  -1, 325, 326, 327,  -1,  -1,  -1,  -1, 328,
      329, 330,  -1, 331,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1, 332,  -1,  -1,
      333, 334,  -1,  -1,  -1, 335,  -1,  -1, 336, 337,
      338, 339,  -1, 340,  -1,  -1, 341,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1, 342,  -1,  -1,  -1,  -1,  -1, 343,
      344,  -1, 345,  -1, 346,  -1,  -1,  -1,  -1, 347,
       -1,  -1,  -1, 348,  -1, 349,  -1,  -1, 350, 351,
      352,  -1, 353, 354,  -1,  -1,  -1,  -1,  -1,  -1,
      355, 356, 357,  -1, 358, 359,  -1, 360,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      361,  -1,  -1,  -1, 362,  -1, 363,  -1,  -1, 364,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      365,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      366, 367,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1, 368,  -1, 369,
       -1, 370, 371,  -1,  -1, 372,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 373,
       -1,  -1,  -1,  -1, 374,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1, 375,  -1,  -1,  -1,  -1,  -1,  -1,
      376,  -1,  -1,  -1, 377,  -1,  -1,  -1,  -1,  -1,
       -1,  -1, 378,  -1, 379,  -1, 380,  -1, 381,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1, 382,  -1, 383,  -1,  -1, 384, 385,  -1,  -1,
       -1,  -1,  -1, 386,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 387,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 388,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1, 389,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 390,
       -1, 391, 392, 393,  -1, 394,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 395,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1, 396,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1, 397,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1, 398,  -1, 399,  -1,  -1,  -1,  -1,
       -1,  -1,  -1, 400,  -1, 401,  -1,  -1,  -1, 402,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1, 403,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1, 404,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 405,  -1,  -1,  -1,
       -1,  -1, 406,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 407,  -1,  -1,  -1,
      408,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1, 409, 410,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 411,  -1,  -1,  -1,
       -1, 412,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1, 413
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
        }
    }
  return 0;
}
