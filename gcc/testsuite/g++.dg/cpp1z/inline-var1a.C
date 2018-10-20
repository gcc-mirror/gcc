// { dg-do compile { target c++17 } }
// { dg-options "-Wno-deprecated -g" }

#include "inline-var1.h"

static inline int var19 = bar (16);
static int inline var20 = bar (17);
inline int var21 = foo (6);
inline int var22 = foo (7);
extern inline int var23;   
inline int var23 = foo (8);

int &alt1 = var1;
int &alt2 = N::var2;   
const int &alt3 = S::var3;
int &alt4 = S::var4;   
const int &alt5 = S::var5;
const int &alt6 = N::var6;
int &alt7 = var7;
double &alt8 = N::var8;
double &alt9 = S::var9; 
const int &alt11 = S::var11;
int &alt12 = var12;
int &alt13 = var13;
int &alt14 = U::var14;
T &alt15 = U::var15;
T &alt16 = U::var16;
int &alt17 = U::var17;
const double &alt18 = U::var18;
int &alt19 = var19;
int &alt20 = var20;
int &alt21 = var21;
int &alt22 = var22;
int &alt23 = var23;
const int &alt24 = Y<int>::var24;
int &alt25 = Y<int>::var25;
int &alt26 = Y<int>::var26;
int &alt27 = var27<int>;
const int &alt28 = Y<int>::var28;
const char &alt24a = Y<char>::var24;
char &alt25a = Y<char>::var25;
int &alt26a = Y<char>::var26;
char &alt27a = var27<char>;
const char &alt28a = Y<char>::var28;
