! { dg-do compile }
!
! PR fortran/30940
program test
implicit none
interface
  subroutine foo(a)
     character(len=1),dimension(:) :: a
  end subroutine foo
  subroutine bar(a)
     character(len=1),dimension(:,:) :: a
  end subroutine bar
  subroutine foobar(a)
     character(len=1),dimension(4) :: a
  end subroutine foobar
  subroutine arr(a)
     character(len=1),dimension(1,2,1,2) :: a
  end subroutine arr
end interface
  character(len=2) :: len2
  character(len=4) :: len4
  len2 = '12'
  len4 = '1234'

  call foo(len2) ! { dg-error "Rank mismatch in argument" }
  call foo("ca") ! { dg-error "Rank mismatch in argument" }
  call bar("ca") ! { dg-error "Rank mismatch in argument" }
  call foobar(len2) ! { dg-error "contains too few elements" }
  call foobar(len4)
  call foobar("bar") ! { dg-error "contains too few elements" }
  call foobar("bar33")
  call arr(len2) ! { dg-error "contains too few elements" }
  call arr(len4)
  call arr("bar") ! { dg-error "contains too few elements" }
  call arr("bar33")
end program test
