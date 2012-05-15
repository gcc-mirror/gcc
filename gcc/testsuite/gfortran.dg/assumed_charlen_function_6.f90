! { dg-do compile }

! PR fortran/41615
! Output nicer error message for invalid assumed-len character function result
! depending on what kind of contained procedure it is.

module funcs
   implicit none
contains
      function assumed_len(x) ! { dg-error "module procedure" }
         character(*) assumed_len
         integer, intent(in) :: x
      end function assumed_len
end module funcs

module mod2
  implicit none
contains
  subroutine mysub ()
    contains
      function assumed_len(x) ! { dg-error "internal function" }
         character(*) assumed_len
         integer, intent(in) :: x
      end function assumed_len
  end subroutine
end module mod2

program main
  implicit none
contains
      function assumed_len(x) ! { dg-error "internal function" }
         character(*) assumed_len
         integer, intent(in) :: x
      end function assumed_len
end program main
