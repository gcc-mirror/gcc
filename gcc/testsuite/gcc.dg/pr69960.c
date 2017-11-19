/* PR c/69960 */
/* { dg-do compile { target int32plus } } */

#define TOLOWER(x) (x&~0x20)
#define Word(s) \
  s[1] ? s[2] ? s[3] ? \
    (TOLOWER(s[0]) << 24) + (TOLOWER(s[1]) << 16) + (TOLOWER(s[2]) << 8) + TOLOWER(s[3]) : \
    (TOLOWER(s[0]) << 16) + (TOLOWER(s[1]) << 8) + TOLOWER(s[2]) : \
    (TOLOWER(s[0]) << 8) + TOLOWER(s[1]) : \
    TOLOWER(s[0])

const unsigned int _the = Word("the");
