! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
subroutine foo
   character(:), allocatable :: x[:]
   character(:), dimension(:), allocatable :: c[:]
   associate (y => x(:)(2:)) ! { dg-error "Unexpected array/substring ref|Invalid association target" }
   end associate ! { dg-error "Expecting END SUBROUTINE" }
   associate (a => c(:)(:)(2:)) ! { dg-error "Unexpected array/substring ref|Invalid association target" }
   end associate ! { dg-error "Expecting END SUBROUTINE" }
end

subroutine bar
   character(:), allocatable :: x[:]
   character(:), allocatable :: c
   
   associate (y => x(:)(:)) ! { dg-error "Unexpected array/substring ref|Invalid association target" }
   end associate ! { dg-error "Expecting END SUBROUTINE" }
   c = x(:)(2:5) ! { dg-error "Unexpected array/substring ref" }
end
