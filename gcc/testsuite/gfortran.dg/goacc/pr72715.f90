program p
  integer :: i
  !$acc loop
  do concurrent (i=1:3) ! { dg-error "ACC LOOP cannot be a DO CONCURRENT loop" }
  end do
end program p
