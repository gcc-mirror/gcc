! { dg-do compile }
!
! Test the fix for pr71883, in which an ICE would follow the error.
!
! Contributed by Gerhard Steinmetz  <gerhard.steinmetz.fortran@t-online.de>
!
program p
   character(3), allocatable :: z(:,:)
   z(1:2,1:2) = 'abc'
   z(2,1) = z(12) ! { dg-error "Rank mismatch in array reference" }
   z(21) = z(1,2) ! { dg-error "Rank mismatch in array reference" }
contains
   subroutine a
      character(3), allocatable :: z(:,:)
      z(1:2,1:2) = 'abc'
      z(2,1) = z(-1) ! { dg-error "Rank mismatch in array reference" }
      z(2,1) = z(99) ! { dg-error "Rank mismatch in array reference" }
      z(2,1) = z(huge(0)) ! { dg-error "Rank mismatch in array reference" }
      z(2,1) = z(-huge(0)) ! { dg-error "Rank mismatch in array reference" }
      z(-1) = z(2,1) ! { dg-error "Rank mismatch in array reference" }
      z(99) = z(2,1) ! { dg-error "Rank mismatch in array reference" }
      z(huge(0)) = z(2,1) ! { dg-error "Rank mismatch in array reference" }
      z(-huge(0)) = z(2,1) ! { dg-error "Rank mismatch in array reference" }
  end subroutine

   subroutine b
      character(:), allocatable :: z(:,:)
      z(1:2,1:2) = 'abc'
      z(2,1) = z(-1) ! { dg-error "Rank mismatch in array reference" }
      z(2,1) = z(99) ! { dg-error "Rank mismatch in array reference" }
      z(2,1) = z(huge(0)) ! { dg-error "Rank mismatch in array reference" }
      z(2,1) = z(-huge(0)) ! { dg-error "Rank mismatch in array reference" }
      z(-1) = z(2,1) ! { dg-error "Rank mismatch in array reference" }
      z(99) = z(2,1) ! { dg-error "Rank mismatch in array reference" }
     z(huge(0)) = z(2,1) ! { dg-error "Rank mismatch in array reference" }
      z(-huge(0)) = z(2,1) ! { dg-error "Rank mismatch in array reference" }
   end subroutine
end
