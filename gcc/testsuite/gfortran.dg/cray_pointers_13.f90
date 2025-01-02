! { dg-do run }
! { dg-additional-options "-fcray-pointer" }
!
! PR fortran/106692 - Cray pointer comparison wrongly optimized away
!
! Contributed by Marek Polacek

program test
  call test_cray()
  call test_cray2()
end

subroutine test_cray()
  pointer(ptrzz1 , zz1)
  ptrzz1=0
  if (ptrzz1 .ne. 0) then
    print *, "test_cray: ptrzz1=", ptrzz1
    stop 1
  else
    call shape_cray(zz1)
  end if
end

subroutine shape_cray(zz1)
  pointer(ptrzz , zz)
  ptrzz=loc(zz1)
  if (ptrzz .ne. 0) then
    print *, "shape_cray: ptrzz=", ptrzz
    stop 3
  end if
end

subroutine test_cray2()
  pointer(ptrzz1 , zz1)
  ptrzz1=0
  if (0 == ptrzz1) then
    call shape_cray2(zz1)
  else
    print *, "test_cray2: ptrzz1=", ptrzz1
    stop 2
  end if
end

subroutine shape_cray2(zz1)
  pointer(ptrzz , zz)
  ptrzz=loc(zz1)
  if (.not. (0 == ptrzz)) then
    print *, "shape_cray2: ptrzz=", ptrzz
    stop 4
  end if
end
