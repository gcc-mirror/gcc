! { dg-do run }
program nint_1
  if (int(anint(8388609.0)) /= 8388609) call abort
  if (int(anint(0.49999997)) /= 0) call abort
  if (nint(8388609.0) /= 8388609) call abort
  if (nint(0.49999997) /= 0) call abort
  if (int(dnint(4503599627370497.0d0),8) /= 4503599627370497_8) call abort
  if (int(dnint(0.49999999999999994d0)) /= 0) call abort
  if (int(anint(-8388609.0)) /= -8388609) call abort
  if (int(anint(-0.49999997)) /= 0) call abort
  if (nint(-8388609.0) /= -8388609) call abort
  if (nint(-0.49999997) /= 0) call abort
  if (int(dnint(-4503599627370497.0d0),8) /= -4503599627370497_8) call abort
  if (int(dnint(-0.49999999999999994d0)) /= 0) call abort
end program nint_1
