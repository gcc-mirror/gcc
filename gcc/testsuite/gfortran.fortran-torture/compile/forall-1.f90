  integer i, a(1)
  logical(kind=8) s(1)

  s = .true.
  a = 42
  forall (i=1:1, .not. s(1)) a(i) = 0
  end
