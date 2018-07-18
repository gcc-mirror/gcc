! { dg-do compile }
!
! PR fortran/30940
program test
implicit none
interface
  subroutine foobar(x)
     integer,dimension(4) :: x
  end subroutine foobar
  subroutine arr(y)
     integer,dimension(1,2,1,2) :: y
  end subroutine arr
end interface

integer a(3), b(5)
call foobar(a) ! { dg-error "contains too few elements" }
call foobar(b)
call foobar(b(1:3)) ! { dg-error "contains too few elements" }
call foobar(b(1:5))
call foobar(b(1:5:2)) ! { dg-error "contains too few elements" }
call foobar(b(2))
call foobar(b(3)) ! { dg-error "Actual argument contains too few elements" }
call foobar(reshape(a(1:3),[2,1])) ! { dg-error "contains too few elements" }
call foobar(reshape(b(2:5),[2,2]))

call arr(a) ! { dg-error "contains too few elements" }
call arr(b)
call arr(b(1:3)) ! { dg-error "contains too few elements" }
call arr(b(1:5))
call arr(b(1:5:2)) ! { dg-error "contains too few elements" }
call arr(b(2))
call arr(b(3)) ! { dg-error "contains too few elements" }
call arr(reshape(a(1:3),[2,1])) ! { dg-error "contains too few elements" }
call arr(reshape(b(2:5),[2,2]))
end program test
