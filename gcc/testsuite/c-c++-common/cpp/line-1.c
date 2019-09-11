/* { dg-do preprocess } */
/* { dg-additional-options -Wno-pedantic } */

main-1 __FILE__

# 7 "inner.h" 1
inner-1 __FILE__
# 9 "inside.h" 1
inside-1 __FILE__
# 11 "" 2
inner-2 __FILE__
#13 "" 2
main-2 __FILE__


/* { dg-final { scan-file line-1.i "main-1 \"\[^\n]*line-1.c\"\n" } } */
/* { dg-final { scan-file line-1.i "main-2 \"\[^\n]*line-1.c\"\n" } } */
/* { dg-final { scan-file line-1.i "inner-1 \"inner.h\"\n" } } */
/* { dg-final { scan-file line-1.i "inner-2 \"inner.h\"\n" } } */
/* { dg-final { scan-file line-1.i "inside-1 \"inside.h\"\n" } } */
