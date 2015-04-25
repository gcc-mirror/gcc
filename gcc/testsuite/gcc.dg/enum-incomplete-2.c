/* PR c/52085 */
/* { dg-do compile } */
/* { dg-options "" } */

#define SA(X) _Static_assert((X),#X)

enum e1;
enum e1 { A } __attribute__ ((__packed__));
enum e2 { B } __attribute__ ((__packed__));
SA (sizeof (enum e1) == sizeof (enum e2));
SA (_Alignof (enum e1) == _Alignof (enum e2));

enum e3;
enum e3 { C = 256 } __attribute__ ((__packed__));
enum e4 { D = 256 } __attribute__ ((__packed__));
SA (sizeof (enum e3) == sizeof (enum e4));
SA (_Alignof (enum e3) == _Alignof (enum e4));

enum e5;
enum e5 { E = __INT_MAX__ } __attribute__ ((__packed__));
enum e6 { F = __INT_MAX__ } __attribute__ ((__packed__));
SA (sizeof (enum e5) == sizeof (enum e6));
SA (_Alignof (enum e5) == _Alignof (enum e6));

enum e7;
enum e7 { G } __attribute__ ((__mode__(__byte__)));
enum e8 { H } __attribute__ ((__mode__(__byte__)));
SA (sizeof (enum e7) == sizeof (enum e8));
SA (_Alignof (enum e7) == _Alignof (enum e8));

enum e9;
enum e9 { I } __attribute__ ((__packed__, __mode__(__byte__)));
enum e10 { J } __attribute__ ((__packed__, __mode__(__byte__)));
SA (sizeof (enum e9) == sizeof (enum e10));
SA (_Alignof (enum e9) == _Alignof (enum e10));

enum e11;
enum e11 { K } __attribute__ ((__mode__(__word__)));
enum e12 { L } __attribute__ ((__mode__(__word__)));
SA (sizeof (enum e11) == sizeof (enum e12));
SA (_Alignof (enum e11) == _Alignof (enum e12));
