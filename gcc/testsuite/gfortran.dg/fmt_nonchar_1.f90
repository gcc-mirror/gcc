! { dg-do compile }
!
! PR fortran/99111
!
program p
   use iso_c_binding
   implicit none
   type t
      integer :: a(1)
   end type
   type(t), parameter :: x(3) = [t(transfer('("he', 1)), &
                                 t(transfer('llo ', 1)), &
                                 t(transfer('W1")', 1))]
   type t2
     procedure(), pointer, nopass :: ppt
   end type t2
   type(t2) :: ppcomp(1)
   interface
     function fptr()
       procedure(), pointer :: fptr
     end function
   end interface
   class(t), allocatable :: cl(:)
   type(c_ptr) :: cptr(1)
   type(c_funptr) :: cfunptr(1)
   procedure(), pointer :: proc
   external proc2

   print x ! { dg-error "Non-character non-Hollerith in FORMAT tag" }
   print cl ! { dg-error "Non-character non-Hollerith in FORMAT tag" }
   print cptr ! { dg-error "Non-character non-Hollerith in FORMAT tag" }
   print cfunptr ! { dg-error "Non-character non-Hollerith in FORMAT tag" }

   print proc ! { dg-error "Syntax error in PRINT statement" }
   print proc2 ! { dg-error "Syntax error in PRINT statement" }
   print ppcomp%ppt ! { dg-error "Syntax error in PRINT statement" }

   print fptr() ! { dg-error "must be of type default-kind CHARACTER or of INTEGER" }

   call bar(1)
contains
   subroutine bar (xx)
     type(*) :: xx
     print xx  ! { dg-error "Assumed-type variable xx at ... may only be used as actual argument" }
   end
end
