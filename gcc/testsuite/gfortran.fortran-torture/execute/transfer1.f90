program chop
  integer ix, iy
  real x, y
  x = 1.
  y = x
  ix = transfer(x,ix)
  iy = transfer(y,iy)
  print '(2z20.8)', ix, iy
  if (ix /= iy) call abort
end program chop
