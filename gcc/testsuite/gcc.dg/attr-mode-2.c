/* PR c/18282 */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int tword __attribute__((mode(word)));

typedef enum { B1 = 1 } B;
typedef enum { C1 = 1 } C __attribute__ ((mode(QI)));
typedef enum { D1 = 1 } __attribute__ ((mode(word))) D;

B __attribute__ ((mode (QI))) bqi;
B __attribute__ ((mode (word))) bword;

int sqi[sizeof (bqi) == 1 ? 1 : -1];
int sword[sizeof (bword) == sizeof(tword) ? 1 : -1];
int sc[sizeof (C) == 1 ? 1 : -1];
int sd[sizeof (D) == sizeof(tword) ? 1 : -1];

int aqi[__alignof (bqi) == 1 ? 1 : -1];
int aword[__alignof (bword) == __alignof(tword) ? 1 : -1];
int ac[__alignof (C) == 1 ? 1 : -1];
int ad[__alignof (D) == __alignof(tword) ? 1 : -1];
