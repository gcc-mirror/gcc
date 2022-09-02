/* { dg-do compile } */
/* { dg-additional-options "-Wuninitialized" } */

int foo (int x, int *val)
{
  switch (x)
    {
#define C(n) \
    case n + 0: return *val; \
    case n + 1: return *val; \
    case n + 2: return *val; \
    case n + 3: return *val; \
    case n + 4: return *val; \
    case n + 5: return *val; \
    case n + 6: return *val; \
    case n + 7: return *val; \
    case n + 8: return *val; \
    case n + 9: return *val;
#define C1(n) \
    C(n+00) C(n+10) C(n+20) C(n+30) C(n+40) \
    C(n+50) C(n+60) C(n+70) C(n+80) C(n+90)
#define C10(n) \
    C1(n+000) C1(n+100) C1(n+200) C1(n+300) C1(n+400) \
    C1(n+500) C1(n+600) C1(n+700) C1(n+800) C1(n+900)
    C10(1000)
    }
  return 0;
}
