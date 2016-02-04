! { dg-do run }
! { dg-additional-options "-fcheck=mem" }
! { dg-shouldfail "Fortran runtime error: Assignment of scalar to unallocated array" }
!
! This omission was encountered in the course of fixing PR54070. Whilst this is a
! very specific case, others such as allocatable components have been tested.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
function g(a) result (res)
  character(len=*) :: a
  character(len=:),allocatable :: res(:)
  res = a  ! Since 'res' is not allocated, a runtime error should occur.
end function

  interface
    function g(a) result(res)
      character(len=*) :: a
      character(len=:),allocatable :: res(:)
    end function
  end interface
  print *, g("ABC")
end
