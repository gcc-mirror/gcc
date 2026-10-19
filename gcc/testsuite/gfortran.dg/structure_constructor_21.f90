! { dg-do run }
!
! PR 125721
!
! A structure constructor with a CLASS array POINTER component associated with
! a target array crashed in gfc_conv_scalarized_array_ref: the pointer
! attribute lives on CLASS_DATA (cm), so no array branch in
! gfc_trans_subcomponent_assign fired and the value reached the scalar/derived
! branch, which converted an array without a scalarizer (se->ss == NULL).
!
! This is the array analog of the scalar class-pointer-component fix.

program main
  implicit none

  type ty
     integer(kind=8) :: i = 1
     integer(kind=8) :: ii = 2
  end type
  type, extends(ty) :: ty1
     integer(kind=8) :: j = 3
     integer(kind=8) :: jj = 4
  end type

  type conArr
     integer :: ty_int
     class(ty), pointer :: ty_class(:,:)
  end type

  type conStr
     class(*), pointer :: u(:)
  end type

  type(conArr) :: obj
  type(conStr) :: cs
  character(5), target :: ca(3) = ["aaaaa", "bbbbb", "ccccc"]
  type(ty1), target :: tar(3,3)

  tar%i = 5
  obj = conArr(7, tar)

  if (obj%ty_int /= 7) stop 1
  if (.not. associated (obj%ty_class)) stop 2
  if (any (shape (obj%ty_class) /= [3,3])) stop 3
  ! Dynamic type is ty1 (8 int8 = 32 bytes per element); 9 elements = 288.
  if (sizeof (obj%ty_class) /= 288) stop 4
  if (obj%ty_class(2,2)%i /= 5) stop 5

  obj%ty_class(2,2)%i = 8
  if (tar(2,2)%i /= 8) stop 6      ! pointer really aliases the target

  ! Unlimited polymorphic array pointer component with a character target.
  cs = conStr(ca)
  select type (q => cs%u)
   type is (character(*))
     if (len (q) /= 5) stop 7
     if (q(2) /= "bbbbb") stop 8
   class default
     stop 9
  end select
end program main
