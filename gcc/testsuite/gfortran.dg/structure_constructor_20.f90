! { dg-do run }
!
! PR 125721
!
! A structure constructor associating a class pointer component (unlimited
! polymorphic or typed) with a target used to ICE in
! gfc_trans_subcomponent_assign (fold_convert_loc), because the pointer
! attribute lives on CLASS_DATA and the code fell through to the derived-type
! branch.  Check that it compiles and performs genuine pointer association.

program p
  type t
     integer :: i = 0
  end type t
  type, extends(t) :: t2
     integer :: j = 0
  end type t2
  type cu
     class(*), pointer :: u
  end type cu
  type ct
     class(t), pointer :: p
  end type ct

  type(t2), target :: tar
  type(cu) :: a
  type(ct) :: b
  character(5), target :: str = "hello"

  ! Unlimited polymorphic pointer component.
  tar%i = 5
  a = cu (tar)
  select type (q => a%u)
   type is (t2)
     if (q%i /= 5) stop 1
   class default
     stop 2
  end select

  ! Typed (parent) class pointer component -> genuine association, not a copy.
  b = ct (tar)
  if (loc (b%p) /= loc (tar)) stop 3
  b%p%i = 9
  if (tar%i /= 9) stop 4

  ! Unlimited polymorphic pointer component with a character target.
  a = cu (str)
  select type (q => a%u)
   type is (character(*))
     if (len (q) /= 5) stop 5
     if (q /= "hello") stop 6
   class default
     stop 7
  end select
end program p
