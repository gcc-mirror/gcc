! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "foo" }
!
! PR 31119
module sub_mod
contains
  elemental subroutine set_optional(i,idef,iopt)
    integer, intent(out)          :: i
    integer, intent(in)           :: idef
    integer, intent(in), optional :: iopt
    if (present(iopt)) then
      i = iopt
    else
      i = idef
    end if
  end subroutine set_optional

  subroutine sub(ivec)
    integer          , intent(in), optional :: ivec(:)
    integer                                 :: ivec_(2)
    call set_optional(ivec_,(/1,2/))
    if (any (ivec_ /= (/1,2/))) call abort
    call set_optional(ivec_,(/1,2/),ivec)
    if (present (ivec)) then
      if (any (ivec_ /= ivec)) call abort
    else
      if (any (ivec_ /= (/1,2/))) call abort
    end if
  end subroutine sub
end module sub_mod

program main
  use sub_mod, only: sub
  call sub()
  call sub((/4,5/))
  call sub((/4/))
end program main
! { dg-output "Fortran runtime error: Array bound mismatch" }
! { dg-final { cleanup-modules "sub_mod" } }
