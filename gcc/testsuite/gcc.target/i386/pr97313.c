/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O2 -fPIE" } */
/* { dg-require-effective-target pie } */

typedef struct {
  int unspecified : 1;
  int secure : 1;
} MemTxAttrs;

enum { MSCAllowNonSecure } tz_msc_read_pdata;

int tz_msc_read_s_0;
int tz_msc_check();
int address_space_ldl_le();

void tz_msc_read(MemTxAttrs attrs) {
  int as = tz_msc_read_s_0;
  long long data;
  switch (tz_msc_check()) {
  case MSCAllowNonSecure:
    attrs.secure = attrs.unspecified = 0;
    data = address_space_ldl_le(as, attrs);
  }
  tz_msc_read_pdata = data;
}
