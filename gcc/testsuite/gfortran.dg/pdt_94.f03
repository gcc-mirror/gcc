! { dg-do run }
!
! Test the fix for PR104776.
!
! Contributed by Amir Shahmoradi  <a.shahmoradi@gmail.com>
!
module pdt_mod

    use iso_fortran_env, only: character_kinds

    integer, parameter :: def_kind = selected_char_kind('ascii')
    integer, parameter :: unicode_kind = selected_char_kind('ISO_10646')
    character(kind=unicode_kind,len=*), parameter :: str = 'abcde'

    type                    :: BG_type(kind_p)
! This is still failing.
        integer     , kind  :: kind_p = character_kinds(1)
!        integer     , kind  :: kind_p = def_kind
        character(5 , kind_p) :: str = achar(97,kind_p)//achar(98,kind_p)// &
                                       achar(99,kind_p)//achar(100,kind_p)//achar(101,kind_p)
    end type

! These declarations worked as expected
!    type (BG_type) :: a_m = BG_type (def_kind)("abcde")
!    type (BG_type(selected_char_kind('ISO_10646'))) :: b_m = BG_type (selected_char_kind('ISO_10646'))(str)
! Now so do these
    type (BG_type) :: a_m
    type (BG_type(unicode_kind)) :: b_m

contains
    subroutine print_mod_vars ()
       if (a_m%str /= 'abcde') stop 1
       if (b_m%str /= str) stop 2
    end
end module

  use pdt_mod
  type (BG_type) :: a  ! Caused an ICE
  type (BG_type(unicode_kind)) :: b
  a_m = BG_type (def_kind)("abcde")
  b_m = BG_type (unicode_kind)(str)
  if (a%str%kind /= def_kind) stop 3
  if (b%str%kind /= unicode_kind) stop 4
  if (a%str /= 'abcde') stop 5
  if (b%str /= str) stop 6
  if (a_m%str /= 'abcde') stop 7
  if (b_m%str /= str) stop 8
  call print_mod_vars ()
end
