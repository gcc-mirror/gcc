program foo
  integer, dimension(3, 2) :: a

  a = reshape ((/1, 2, 3, 4, 5, 6/), (/3, 2/))
  a = a(3:1:-1, 2:1:-1);

  if (any (a .ne. reshape ((/6, 5, 4, 3, 2, 1/), (/3, 2/)))) STOP 1
end program
