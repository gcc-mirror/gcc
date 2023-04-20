/* PR c/107041 */
/* { dg-do compile } */
/* { dg-additional-options "-Wenum-int-mismatch" } */

typedef enum acc_device_t {
  acc_device_current = -1,
  acc_device_none = 0,
  acc_device_default = 1,
  acc_device_host = 2,
  acc_device_not_host = 4,
  acc_device_nvidia = 5,
  acc_device_radeon = 8,
  _ACC_highest = __INT_MAX__
} acc_device_t;

int acc_on_device (acc_device_t);		/* { dg-bogus "conflicting types for 'acc_on_device' due to enum/integer mismatch; have 'int\\\(acc_device_t\\\)'" } */
int acc_on_device (acc_device_t);

int
foo (void)
{
  return acc_on_device (acc_device_host);
}
