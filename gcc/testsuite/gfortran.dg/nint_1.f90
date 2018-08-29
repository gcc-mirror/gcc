! { dg-do run }
program nint_1
  if (int(anint(8388609.0)) /= 8388609) STOP 1
  if (int(anint(0.49999997)) /= 0) STOP 2
  if (nint(8388609.0) /= 8388609) STOP 3
  if (nint(0.49999997) /= 0) STOP 4
  if (int(dnint(4503599627370497.0d0),8) /= 4503599627370497_8) STOP 5
  if (int(dnint(0.49999999999999994d0)) /= 0) STOP 6
  if (int(anint(-8388609.0)) /= -8388609) STOP 7
  if (int(anint(-0.49999997)) /= 0) STOP 8
  if (nint(-8388609.0) /= -8388609) STOP 9
  if (nint(-0.49999997) /= 0) STOP 10
  if (int(dnint(-4503599627370497.0d0),8) /= -4503599627370497_8) STOP 11
  if (int(dnint(-0.49999999999999994d0)) /= 0) STOP 12
end program nint_1
