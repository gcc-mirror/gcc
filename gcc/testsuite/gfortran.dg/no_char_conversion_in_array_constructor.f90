! { dg-do compile }
! { dg-options "-fdec-char-conversions" }

program p
   print *, -[integer :: 1, [integer(8) :: '2']] ! { dg-error "Cannot convert" }
   print *, -[real :: 1, [real(8) :: '2']]       ! { dg-error "Cannot convert" }
   print *, -[complex :: 1, [complex(8) :: '2']] ! { dg-error "Cannot convert" }
   print *, [logical :: 1, [logical(8) :: '2']]  ! { dg-error "Cannot convert" }
end

