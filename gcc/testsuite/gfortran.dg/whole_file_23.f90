! { dg-do compile }
! 
! PR fortran/40873
!
! Failed to compile (segfault) with -fwhole-file.
! Cf. PR 40873 comment 24; test case taken from
! PR fortran/31867 comment 6.
!

pure integer function lensum (words, sep)
  character (len=*), intent(in)        :: words(:), sep
  lensum = (size (words)-1) * len (sep) + sum (len_trim (words))
end function

module util_mod
  implicit none
  interface
    pure integer function lensum (words, sep)
      character (len=*), intent(in)        :: words(:), sep
    end function
  end interface
  contains
  function join (words, sep) result(str)
! trim and concatenate a vector of character variables, 
! inserting sep between them
    character (len=*), intent(in)        :: words(:), sep
    character (len=lensum (words, sep))  :: str
    integer                              :: i, nw
    nw  = size (words)
    str = ""
    if (nw < 1) then
      return
    else
      str = words(1)
    end if
    do i=2,nw
      str = trim (str) // sep // words(i)
    end do
  end function join
end module util_mod
!
program xjoin
  use util_mod, only: join
  implicit none
  character (len=5) :: words(2) = (/"two  ","three"/) 
  write (*,"(1x,'words = ',a)") "'"//join (words, "&")//"'"
end program xjoin

! { dg-final { cleanup-modules "util_mod" } }
