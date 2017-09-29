! { dg-do compile }
!
! PR fortran/30940
program test
implicit none
interface
  subroutine foobar(a)
     character(len=1),dimension(4) :: a
  end subroutine foobar
  subroutine arr(a)
     character(len=1),dimension(1,2,1,2) :: a
  end subroutine arr
end interface

  call foobar( [ "bar" ]) ! { dg-error "contains too few elements" }
  call foobar( ["ba ","r33"])
  call arr( [ "bar" ]) ! { dg-error "contains too few elements" }
  call arr( reshape(["b","a","r","3"], [2,2]))
  call arr( reshape(["b","a"], [1,2])) ! { dg-error "contains too few elements" }
  call arr( reshape(["b","a"], [2,1])) ! { dg-error "contains too few elements" }
end program test
