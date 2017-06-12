/* { dg-do compile } */

/* Used to fail in SLSR because of a dominance violation.  PR80054.  */

extern short var_2;
extern short var_4;
extern const bool var_32;
extern short var_36;
extern const bool var_37;
extern bool var_46;
extern unsigned int var_47;
extern short var_49;
extern unsigned int var_56;
extern unsigned int var_62;
extern unsigned int var_65;
extern bool var_831;
extern unsigned int var_843;
extern short var_846;
extern short var_889;

void foo() {
  if (var_36 * var_37)
    var_831 = var_56 = 0;
  else
    var_65 = 0;

  if (var_46)
    var_843 = 0;

  var_846 = 0;

  if ((var_4 == 0) >> (var_32 | -(var_37 < var_46 || var_36)) + 8)
    var_49 = 2032651381 * bool(var_2 * var_37);
  else {
    var_62 = 0;
    var_47 = (var_46 || var_36) * (var_2 * var_37);
  }

  var_889 = bool(var_2 * var_37);
}
