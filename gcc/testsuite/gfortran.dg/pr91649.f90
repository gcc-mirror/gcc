! { dg-do compile }
! PR fortran/91649
! Code originally contributed by Gerhard Steinmetz
subroutine p
   logical :: back = .true.
   integer :: x(1) = findloc([1, 2, 1], '1', back=back) ! { dg-error "must be in type conformance" }
   print *, x
end

subroutine q
   type t
   end type
   logical :: back = .false.
   integer :: x(1) = findloc([1, 2, 1], t(), back=back) ! { dg-error "must be of intrinsic type" }
   print *, x
end

subroutine s
   character(4) :: c = '1234'
   integer :: x(1) = findloc([1, 2, 1], c, back=.true.) ! { dg-error "must be in type conformance" }
   print *, x
end

