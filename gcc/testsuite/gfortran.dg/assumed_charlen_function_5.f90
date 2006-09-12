! { dg-do compile }
! Tests the patch for PR28890, in which a reference to a legal reference
! to an assumed character length function, passed as a dummy, would
! cause an ICE.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
character(*) function charrext (n)  ! { dg-warning "is obsolescent in fortran 95" }
  character(26) :: alpha ="abcdefghijklmnopqrstuvwxyz"
  charrext = alpha (1:n)
end function charrext

  character(26), external :: charrext
  interface
    integer(4) function test(charr, i)
     character(*), external :: charr
     integer :: i
    end function test
  end interface

  do j = 1 , 26
    m = test (charrext, j)
    m = ctest (charrext, 27 - j)
  end do
contains
  integer(4) function ctest(charr, i)  ! { dg-warning "is obsolescent in fortran 95" }
    character(*) :: charr
    integer :: i
    print *, charr(i)
    ctest = 1
  end function ctest
end

integer(4) function test(charr, i)  ! { dg-warning "is obsolescent in fortran 95" }
  character(*) :: charr
  integer :: i
  print *, charr(i)
  test = 1
end function test
