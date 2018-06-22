! PR middle-end/85878

program pr85878
  real :: a
  complex :: c = (2.0, 3.0)
  print *, c
  print *, transfer (a, c)
end
