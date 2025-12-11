! { dg-do compile }
!
! Test the fix for PR107142, which used to ICE after a syntax error.
!
! Contributed by Arseny Solokha  <asolokha@gmx.com>
!
module c1162a
  type pdt(kind,len)
    integer, kind :: kind
    integer, len :: len
  end type
 contains
  subroutine foo(x)
    class(pdt(kind = 1, len = :)), allocatable :: x
    select type (x)
      type is (pdt(kind = *, len = *)) ! { dg-error "does not have a default value" }
      type is (pdt(kind = :, len = *)) ! { dg-error "does not have a default value" }
    end select
    select type (x)
      type is (pdt(kind = 1, len = *)) ! This, of course, is OK
    end select
  end subroutine
end module
