/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-clones"  } */

typedef struct rtx_def *rtx;
enum rtx_code {
  LAST_AND_UNUSED_RTX_CODE};
extern const char * const rtx_format[((int) LAST_AND_UNUSED_RTX_CODE)];
struct rtx_def {
  enum rtx_code code;
};
typedef int (*rtx_function) (rtx *, void *);
extern int for_each_rtx (rtx *, rtx_function, void *);
int
replace_label (rtx *x, void *data)
{
  rtx l = *x;
  if (l == (rtx) 0)
    {
 {
   rtx new_c, new_l;
   for_each_rtx (&new_c, replace_label, data);
 }
    }
}
static int
for_each_rtx_1 (rtx exp, int n, rtx_function f, void *data)
{
  int result, i, j;
  const char *format = (rtx_format[(int) (((enum rtx_code) (exp)->code))]);
  rtx *x;
  for (; format[n] != '\0'; n++)
    {
      switch (format[n])
 {
 case 'e':
   result = (*f) (x, data);
     {
       result = for_each_rtx_1 (*x, i, f, data);
     }
 }
    }
}
int
for_each_rtx (rtx *x, rtx_function f, void *data)
{
  int i;
  return for_each_rtx_1 (*x, i, f, data);
}

/* { dg-final { scan-ipa-dump-not "(null)"  "ipa-clones"  } } */
