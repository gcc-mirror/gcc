program badarray
  implicit none
  integer:: j(3) = [1,2,3]
  call doubling(j)
contains
  subroutine doubling(  n)
    integer,intent(in)::n(:)
    integer::m = size(n)      ! { dg-error "Assumed-shape array" }
    print *, m                ! { dg-error "has no IMPLICIT type" }
  end subroutine doubling
end program badarray
