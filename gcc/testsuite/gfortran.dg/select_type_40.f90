! { dg-do compile }
! { dg-additional-options "-fdefault-integer-8" }
! PR 78238 - this used to cause an ICE.
! Original test cae by Gerhard Steinmetz
class(*), allocatable :: q
select type (x => q)
type is (real)
end select
end
