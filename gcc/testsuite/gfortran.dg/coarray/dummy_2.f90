! { dg-do compile }
!
! PR fortran/99817
!
! Contributed by G. Steinmetz
!
subroutine s1 (x)
   character(*) :: x(*)[*]
end

subroutine s2 (x)
   character(*), dimension(*), codimension[*] :: x
   integer :: i
   i = len(x)
end

subroutine s3 (x, y)
   character(*), dimension(:) :: x[*]
   character(*) :: y
end

subroutine s4 (x, y, z)
   character(*), dimension(:) :: x[2, *]
   character(*), dimension(*) :: y
   character(*) :: z
end
