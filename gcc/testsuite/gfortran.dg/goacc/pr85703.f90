! PR fortran/85703
! { dg-do compile }

character function f()
  !$acc parallel loop reduction(+:a)
  do i = 1, 4
  end do
  !$acc end parallel loop
end
