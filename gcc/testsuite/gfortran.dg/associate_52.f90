! { dg-do compile }
!
! PR fortran/93427
!
! Contributed by Andrew Benson
!
module a

type :: t
end type t

contains

recursive function b()
  class(t), pointer :: b
  type(t) :: c
  allocate(t :: b)
  select type (b)
  type is (t)
     b=c
  end select
end function b

end module a
