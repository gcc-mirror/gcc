! { dg-do compile }
!
! PR fortran/48810
!
! Contributed by Andrew Baldwin
!
      module qtest
      type foobar
        integer :: x
        contains
        private
        procedure :: gimmex
        generic, public :: getx => gimmex
      end type foobar
      contains
        function gimmex(foo)
          class (foobar) :: foo
          integer :: gimmex
          gimmex = foo%x
        end function gimmex
      end module qtest

      module qtestPriv
      type foobarPriv
        integer :: x
        contains
        private
        procedure :: gimmexPriv
        generic, private :: getxPriv => gimmexPriv
      end type foobarPriv
      contains
        function gimmexPriv(foo)
          class (foobarPriv) :: foo
          integer :: gimmex
          gimmex = foo%x
        end function gimmexPriv
      end module qtestPriv

      program quicktest
      use qtest
      use qtestPriv
      type (foobar) :: foo
      type (foobarPriv) :: fooPriv
      integer :: bar
      bar = foo%getx()  ! OK
      bar = fooPriv%getxPriv() ! { dg-error " is PRIVATE " }
      end program quicktest

! { dg-final { cleanup-modules "qtest qtestpriv" } }
