! { dg-do run }
!
! Test the fix for PR109345 in which array references in the SELECT TYPE
! block below failed because the descriptor span was not set correctly.
!
! Contributed by Lauren Chilutti  <lchilutti@gmail.com>
!
program test
  implicit none
  type :: t
    character(len=12, kind=4) :: str_array(4)
    integer :: i
  end type
  character(len=12, kind=1), target :: str_array(4)
  character(len=12, kind=4), target :: str_array4(4)
  type(t) :: str_t (4)
  integer :: i

  str_array(:) = ""
  str_array(1) = "12345678"
  str_array(2) = "@ABCDEFG"
! Original failing test
  call foo (str_array)

  str_array4(:) = ""
  str_array4(1) = "12345678"
  str_array4(2) = "@ABCDEFG"
  str_t = [(t(str_array4, i), i = 1, 4)]
! Test character(kind=4)
  call foo (str_t(2)%str_array)
! Test component references
  call foo (str_t%str_array(1), .true.)
! Test component references and that array offset is correct.
  call foo (str_t(2:3)%i)

contains
  subroutine foo (var, flag)
    class(*), intent(in) :: var(:)
    integer(kind=4) :: i
    logical, optional :: flag
    select type (var)
    type is (character(len=*, kind=1))
       if (len (var) /= 12) stop 1
! Scalarised array references worked.
       if (any (var /= str_array)) stop 2
       do i = 1, size(var)
! Elemental array references did not work.
          if (trim (var(i)) /= trim (str_array(i))) stop 3
       enddo

    type is (character(len=*, kind=4))
       if (len (var) /= 12) stop 4
! Scalarised array references worked.
       if (any (var /= var(1))) then
         if (any (var /= str_array4)) stop 5
       else
         if (any (var /= str_array4(1))) stop 6
       end if
       do i = 1, size(var)
! Elemental array references did not work.
          if (var(i) /= var(1)) then
            if (present (flag)) stop 7
            if (trim (var(i)) /= trim (str_array4(i))) stop 8
          else
            if (trim (var(i)) /= trim (str_array4(1))) stop 9
          end if
       enddo

       type is (integer(kind=4))
         if (any(var /= [2,3])) stop 10
         do i = 1, size (var)
           if (var(i) /= i+1) stop 11
         end do
    end select
  end
end

