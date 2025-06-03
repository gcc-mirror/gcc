! { dg-do compile }
! { dg-additional-options "-fcoarray=lib -Warray-temporaries" }
!
! PR fortran/99838 - ICE due to missing locus with data statement for coarray
!
! Contributed by Gerhard Steinmetz

program p
  type t
     integer :: a
  end type
  type(t) :: x(3)[*]
  data x%a /1, 2, 3/ ! { dg-warning "Creating array temporary" }
end
