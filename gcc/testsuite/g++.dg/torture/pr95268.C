// { dg-do compile }
// { dg-require-effective-target lp64 }
// { dg-additional-options "-Wno-overflow" }

#include <algorithm>

extern short var_0, var_2, var_3, var_9, var_11, var_13, var_14, var_19, var_22,
    var_32, var_37, var_44, var_57, var_59, var_63, var_70;
extern unsigned var_5;
extern char var_6, var_12, var_18, var_38, var_39, var_43, var_55, var_64,
    arr_35;
extern long var_7, var_8, var_10, var_15, var_25, var_56;
extern int var_21, var_36, var_51, var_65, var_68, arr_7;
extern bool var_46, var_58, var_67;

void test() {
  var_12 = 0 >= 0;
  var_13 = arr_7;
  var_14 = (unsigned long)var_7 >> -564810131 + 564810189;
  var_15 = var_5;
  var_18 = -602739307623583391;
  var_19 = -~0;
  var_21 = var_10 >> var_8 - 17101301574577641170ULL;
  var_22 = var_5;
  var_25 = var_9;
  var_32 = std::max((unsigned)var_2, var_5);
  var_36 = 9557;
  var_37 = 394545925;
  var_38 = 0 >= 0;
  var_39 = var_5;
  var_43 = 0;
  var_44 = arr_35;
  var_46 = var_7;
  for (short a = 0; a < 9; a = 021)
    for (short b = 0; b < 024; b += 4)
      var_51 = std::min((long long)(var_2 ?: var_9), (long long)var_9);
  var_55 = var_0;
  var_56 = 3896150587;
  var_57 = var_11;
  var_58 = var_59 = var_11;
  var_63 = 73;
  var_64 = 10393232284806619711ULL;
  var_65 = var_3;
  var_67 = var_6;
  var_68 = var_70 = 0;
}
