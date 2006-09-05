! { dg-do compile }
! Tests the fix for a further regression caused by the
! fix for PR28788 and posted as PR28908. The problem was
! caused by the patch preventing interface derived types
! from associating with identical derived types in the
! containing namespaces.
!
! Contributed by HJ Lu  <hjl@lucon.org>
!
module bar
  implicit none
  public
  type domain_ptr
    type(domain), POINTER  :: ptr
  end type domain_ptr
  type domain
    TYPE(domain_ptr) , DIMENSION( : ) , POINTER         :: parents
    TYPE(domain_ptr) , DIMENSION( : ) , POINTER         :: nests
  end type domain
end module bar

module foo
contains
  recursive subroutine integrate (grid)
    use bar
    implicit none
    type(domain), POINTER  :: grid
    interface
      subroutine solve_interface (grid)
        use bar
        TYPE (domain) grid
      end subroutine solve_interface
    end interface
  end subroutine integrate
end module foo
! { dg-final { cleanup-modules "foo bar" } }
