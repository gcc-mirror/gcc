! { dg-do compile }
!
! PR 41781: [OOP] bogus undefined label error with SELECT TYPE.
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>
! and Tobias Burnus >burnus@gcc.gnu.org>

! 1st example: jumping out of SELECT TYPE (valid)
type bar
  integer :: i
end type bar
class(bar), pointer :: var
select type(var)
class default
  goto 9999
end select
9999 continue

! 2nd example: jumping out of BLOCK (valid) 
block
  goto 88
end block
88 continue

! 3rd example: jumping into BLOCK (invalid)
goto 99        ! { dg-error "is not in the same block" }
block
  99 continue  ! { dg-error "is not in the same block" }
end block

end
