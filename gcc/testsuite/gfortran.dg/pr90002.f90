! { dg-do compile }
! { dg-options "-fcoarray=single" }
! Contributed by Arseny Solokha <asolokha at gmx dot de>
module pc
  integer, dimension(1) :: zw[1:1,1:*]
end module pc
