! { dg-do compile }
! PR fortran/93580

program p
   integer, parameter :: n = 4
   complex(n%re) :: x    ! { dg-error "The RE or IM part_ref at" }
   complex(n%im) :: y    ! { dg-error "The RE or IM part_ref at" }
   complex(n%len) :: z   ! { dg-error "The LEN part_ref at" }
   character(n%im) :: a  ! { dg-error "The RE or IM part_ref at" }
   character(n%re) :: b  ! { dg-error "The RE or IM part_ref at" }
   character(n%len) :: c ! { dg-error "The LEN part_ref at" }
end

