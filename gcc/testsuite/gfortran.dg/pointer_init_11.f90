! { dg-do compile }
!
! PR 85537: [F08] Invalid memory reference at runtime when calling subroutine through procedure pointer
!
! Contributed by Tiziano Müller <dev-zero@gentoo.org>

module m1
    implicit none
contains
    subroutine foo()
      integer :: a

      abstract interface
        subroutine ibar()
        end subroutine
      end interface

      procedure(ibar), pointer :: bar_ptr => bar_impl  ! { dg-error "invalid in procedure pointer initialization" }

    contains
      subroutine bar_impl()
        write (*,*) "foo"
        a = a + 1
      end subroutine

    end subroutine
end module


module m2
    implicit none
contains
    subroutine foo(dbar)
      interface
        subroutine dbar()
        end subroutine
      end interface

      procedure(dbar), pointer :: bar_ptr => dbar  ! { dg-error "invalid in procedure pointer initialization" }

      call bar_ptr()

    end subroutine
end module
