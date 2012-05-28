// PR c++/25137
// { dg-options "-Wall" }

struct S { int s[3]; };
S s1 = { 1, 1, 1 };

struct S1 { int s[3]; };
struct S2 { struct S1 a; };
S2 s21 = { 1, 1, 1 }; 

struct S3 { int s[3]; };
struct S4 { struct S3 a; int b; };
S4 s41 = { 1, 1, 1, 1 };

struct S5 { int s[3]; };
struct S6 { struct S5 a; int b; };
S6 s61 = { { 1, 1, 1 }, 1 };

struct S7 { int s[3]; };
struct S8 { int a; struct S7 b; };
S8 s81 = { 1, { 1, 1, 1 } };

struct S9 { int s[2]; };
struct S10 { struct S9 a; struct S9 b; };
S10 s101 = { { 1, 1 }, 1, 1 };

struct S11 { int s[2]; };
struct S12 { struct S11 a; struct S11 b; };
S12 s121 = { { 1, 1 }, { 1, 1 } };

struct S13 { int i; };
struct S14 { struct S13 a; };
struct S15 { struct S14 b; };
S15 s151 = { 1 };
