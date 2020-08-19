! { dg-do compile }
!
! Test the fix for PR96102 in which the two lines with errors previously
! caused a segfault.
!
! Contributed by Gerhardt Steinmetz  <gscfq@t-online.de>
!
!
module m
   type mytype
     integer :: i
   end type
   type(mytype) :: d = mytype (42) ! { dg-error "is host associated" }
   integer :: n = 2                ! { dg-error "is host associated" }
contains
   subroutine s
      if ( n /= 0 ) stop 1    ! { dg-error "internal procedure of the same name" }
      if ( d%i /= 0 ) stop 2  ! { dg-error "internal procedure of the same name" }
   contains
      integer function n()
         n = 0
      end
      type(mytype) function d()
         d = mytype (0)
      end
   end
end
